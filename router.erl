-module(router).
-export([start/1]).

start(RouterName) ->
    Pid = spawn(fun() -> loop(RouterName, ets:new(routing, [private,set])) end),
    Pid.

stop(Routing) ->
    ets:delete(Routing),
    exit(normal).

% If the this node's name is the same as the destination of the message respond to Pid
message(Name, _, Name, Pid, Trace) -> Pid ! {trace, self(), lists:append(Trace, [Name])};
% Otherwise, append this router to the trace and pass the message on
message(Name, Routing, Dest, Pid, Trace) ->
    % First, make sure the destination router can be reached
    case ets:lookup(Routing, Dest) of
        [{_, Next}] -> Next ! {message, Dest, self(), Pid, lists:append(Trace, [Name])};
        true        -> Pid  ! {trace, self(), []}
    end.

% If a control message has SeqNum 0, it is an initialization
control(Name, Routing, ControlFun, {Pid, Pid, 0}) ->
    case ControlFun(Name, Routing) of
        % Im not entirely sure what was required when an initialization failed
        abort -> Pid ! {abort, self(), 0};
        _Else -> Pid ! {committed, self(), 0}
    end;

% Otherwise, handle a normal control request
control(Name, Routing, ControlFun, MsgInfo) ->
    % Make a copy of the Routing table in case the request is aborted
    TempRouting = ets:new(temp, [private,set]),
    ets:foldl(fun(E, _) -> ets:insert(TempRouting, E) end, notused, Routing),
    Children = ControlFun(Name, TempRouting),

    % Check whether the control function succeeded
    case Children of
        abort -> waitAbort({Name, Routing, TempRouting, []}, MsgInfo, []);
        _Else -> false
    end,

    % Attempt to propagate the message across the network
    try
        % 2PC phase one, ask neighbours if they can commit
        PidList = checkNeighbours(Routing, ControlFun, MsgInfo),
        % 2PC phase two, wait for a doCommit or doAbort message from root
        waitCommit({Name, Routing, TempRouting}, MsgInfo, PidList)
    catch
        % If at any time you receive an abort message, wait for an "doAbort" message immediately
        throw:{abort,   Pids} -> waitAbort({Name, Routing, TempRouting, Children}, MsgInfo, Pids);
        % If at any time you receive an doAbort message, immediately skip to initiating doAbort
        throw:{doAbort, Pids} -> doAbort({Name, Routing, TempRouting, Children}, MsgInfo, Pids)
    end.

% Propagate a message across all edges checking if all neighbours can commit
checkNeighbours(Routing, ControlFun, {From, Pid, SeqNum}) ->
    PidList = uniquePids(Routing, [], none),

    % propagate the control request around the network, and wait for canCommit or abort responses
    propagate(PidList, fun(Dest) -> 
        case request(From, Dest, SeqNum, {control, self(), Pid, SeqNum, ControlFun}) of
            abort   -> throw({abort,   PidList});
            doAbort -> throw({doAbort, PidList});
            _Else   -> true
        end
    end),

    PidList.

% Create a list of unique PIDs from the Routing Table (so you don't redundant messages)
uniquePids(Routing, [], _) -> 
    [{Name, Pid}] = ets:lookup(Routing, ets:first(Routing)),
    uniquePids(Routing, [Pid], ets:next(Routing, Name));

% If you've already seen the entire list, just return
uniquePids(_, PidList, '$end_of_table') ->
    PidList;

% Recursively iterate over the list adding a PID if it isnt already in the list
uniquePids(Routing, PidList, Name) ->
    [{_, Pid}] = ets:lookup(Routing, Name),
    case lists:member(Pid, PidList) of
        false -> uniquePids(Routing, lists:append(PidList, [Pid]), ets:next(Routing, Name));
        true  -> uniquePids(Routing, PidList, ets:next(Routing, Name))
    end.

% Invoked when canCommit has been received from all neighbours
% If this is the root router (From == Pid) don't wait for a doCommit message
waitCommit(Meta, {Pid, Pid, SeqNum}, PidList) ->
    Pid ! {committed, self(), SeqNum},
    doCommit(Meta, SeqNum, PidList);

% If this isn't the root router, you need to wait for a doCommit message
waitCommit(Meta, {From, _, SeqNum}, PidList) ->
    % Wait for a doCommit or doAbort message
    case request(From, From, SeqNum, {canCommit, self(), SeqNum}) of
        doAbort -> throw({doAbort, PidList});
        abort   -> throw({abort, PidList});
        _Else   -> true
    end,
    doCommit(Meta, SeqNum, PidList).

% (after receiving doCommit) propagate doCommit messages across the network and commit the change
doCommit({Name, Routing, TempRouting}, SeqNum, PidList) ->
    % Propagate the doCommit message across the network
    propagate(PidList, fun(Dest) -> Dest ! {doCommit, self(), SeqNum} end),
    ets:delete(Routing),
    cleanQueue(Name, TempRouting, SeqNum).

% Essentially the same as waitCommit
% If this is the root router, don't wait for a doAbort message
waitAbort(Meta, {Pid, Pid, SeqNum}, PidList) ->
    Pid ! {abort, self(), SeqNum},
    doAbort(Meta, {Pid, Pid, SeqNum}, PidList);

% Otherwise wait for a doAbort message then initiate the abort
waitAbort(Meta, {From, Pid, SeqNum}, PidList) ->
    case request(From, From, SeqNum, {abort, self(), SeqNum}) of
        doAbort -> true;
        _Else   -> io:format("Bad abort. ~w ~n", [self()])
    end,
    doAbort(Meta, {From, Pid, SeqNum}, PidList).

% (After receiving a doAbort message) propagate the doAbort message to neighbours and abort
doAbort({Name, Routing, TempRouting, Children}, {From, _, SeqNum}, PidList)  -> 
    % Propagate a doAbort message to all children
    propagate(PidList, fun(Dest) -> request(From, Dest, SeqNum, {doAbort, self(), SeqNum}) end),
    ets:delete(TempRouting),

    % Kill all of the children created by ControlFun, I assume this is what was intended?
    lists:foldl(fun(ChildPid, _) -> exit(ChildPid, abort) end, notused, Children),

    cleanQueue(Name, Routing, SeqNum).

% Propagate a message across the network, the MSG is provided by the caller in MsgFunc
% If the PidList is empty, just return
propagate([], _) -> true;
% Otherwise send the message to everyone in PidList
propagate(PidList, MsgFunc) ->
    lists:foldl(fun(Pid, _) -> MsgFunc(Pid) end, notused, PidList).

% Send a message to a PID, and return it's reply
request(From, Dest, SeqNum, Message) ->
    Dest ! Message,
    Result = commitloop(Dest, From, SeqNum),
    Result.

% Wait in a loop until a 2PC message is received, ignoring other control messages
commitloop(Dest, From, SeqNum) ->
    receive
        % If you get another control request of the same type, ignore it
        {control, FromPid, _, SeqNum, _}   -> 
            FromPid ! {canCommit, self(), SeqNum},
            commitloop(Dest, From, SeqNum);
        % If you get a control request of a different number, abort it
        {control, FromPid, _, OtherNum, _} ->
            FromPid ! {abort, self(), OtherNum},
            commitloop(Dest, From, SeqNum);
        % First phase messages
        {canCommit, Dest, SeqNum} -> canCommit;
        {abort,     Dest, SeqNum} -> abort;
        % Second phase messages
        {doCommit,  From, SeqNum} -> doCommit;
        {doAbort,   From, SeqNum} -> doAbort
    after 5000 -> abort
    end.

% Remove any messages pertaining to the recently completed 2PC out of the message queue
% before going back into the receive queue
cleanQueue(Name, Routing, SeqNum) ->
    receive
        {control, _, _, SeqNum, _} -> true;
        {canCommit, _, SeqNum}     -> true;
        {abort,     _, SeqNum}     -> true;
        {doCommit,  _, SeqNum}     -> true;
        {doAbort,   _, SeqNum}     -> true
    % Give it 1 seconds to make sure no more will be coming in
    after 10 -> loop(Name, Routing)
    end,

    cleanQueue(Name, Routing, SeqNum).

% Wait for messages to come in, and dispatch them to the correct helper function
% Name:     the name of the router
% Routing:  the routing table for the router
loop(Name, Routing) ->
    receive
        {message, Dest, _, Pid, Trace} ->
            message(Name, Routing, Dest, Pid, Trace);
        {control, From, Pid, SeqNum, ControlFun} ->
            control(Name, Routing, ControlFun, {From, Pid, SeqNum});
        {dump, From} -> From ! {table, self(), ets:match(Routing, '$1')};
        stop         -> stop(Routing)
    end,

    loop(Name, Routing).