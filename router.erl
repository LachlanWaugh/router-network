-module(router).
-export([start/1]).

%
start(RouterName) ->
    Pid = spawn(fun() -> loop(RouterName, ets:new(routing, [private,set])) end),
    % io:format("DEBUG: Node ~w Pid ~w ~n", [RouterName, Pid]),
    Pid.

%
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
control(Name, Routing, _, Pid, 0, ControlFun) ->
    case ControlFun(Name, Routing) of
        abort -> Pid ! {initabort, self(), 0};
        _Else -> Pid ! {initcommitted, self(), 0}
    end;
% Otherwise, handle a normal control request
control(Name, Routing, From, Pid, SeqNum, ControlFun) ->
    TempRouting = ets:new(temp, [private,set]),
    ets:foldl(fun(E, _) -> ets:insert(TempRouting, E) end, notused, Routing),
    Children = ControlFun(Name, TempRouting),

    % Check whether the control function succeeded
    case Children of
        abort -> waitAbort(Name, Routing, TempRouting, From, Pid, SeqNum, []);
        _Else -> false
    end,

    % Attempt to propagate the message across the network
    try
        % 2PC phase one, ask neighbours if they can commit
        PidList = checkNeighbours(Routing, From, Pid, SeqNum, ControlFun),
        % io:format("DEBUG: ~w reached control ~n", [self()]),
        % 2PC phase two, wait for a doCommit or doAbort message from root
        waitCommit(Name, Routing, TempRouting, From, Pid, SeqNum, PidList)
    catch
        % If at any time you receive an abort message, wait for an "aborted" message immediately
        throw:{abort,   Pids} -> waitAbort(Name, Routing, TempRouting, From, Pid, SeqNum, Pids);
        % If at any time you receive an aborted message, immediately skip to initiating doAbort
        throw:{doAbort, Pids} -> doAbort(Name, Routing, TempRouting, From, SeqNum, Pids)
    end.

% Propagate a message across all edges checking if all neighbours can commit
checkNeighbours(Routing, From, Pid, SeqNum, ControlFun) ->
    PidList = uniquePids(Routing, [], none),

    % propagate the control request around the network, and wait for canCommit or abort responses
    propagate(PidList, fun(Dest) -> 
        case request(From, Dest, SeqNum, {control, self(), Pid, SeqNum, ControlFun}) of
            abort   -> throw({abort,   PidList});
            aborted -> throw({aborted, PidList});
            _Else   -> true
        end
    end),

    PidList.

%
uniquePids(Routing, [], _) -> 
    [{Name, Pid}] = ets:lookup(Routing, ets:first(Routing)),
    uniquePids(Routing, [Pid], ets:next(Routing, Name));

% If you've already seen the entire list, just return
uniquePids(_, PidList, '$end_of_table') ->
    PidList;

%
uniquePids(Routing, PidList, Name) ->
    [{_, Pid}] = ets:lookup(Routing, Name),
    case lists:member(Pid, PidList) of
        false -> uniquePids(Routing, lists:append(PidList, [Pid]), ets:next(Routing, Name));
        true  -> uniquePids(Routing, PidList, ets:next(Routing, Name))
    end.

% If this is the root router (From == Pid) don't wait for a doCommit message
waitCommit(Name, Routing, TempRouting, Pid, Pid, SeqNum, PidList) ->
    % io:format("DEBUG: ~w progagating doCommit ~n", [self()]),
    Pid ! {committed, self(), SeqNum},
    doCommit(Name, Routing, TempRouting, SeqNum, PidList);

% If this isn't the root router, you need to wait for a doCommit message
waitCommit(Name, Routing, TempRouting, From, _, SeqNum, PidList) ->
    % io:format("DEBUG: ~w waiting for doCommit ~n", [self()]),

    % Wait for a doCommit or doAbort message
    case request(From, From, SeqNum, {canCommit, self(), SeqNum}) of
        doAbort -> throw({doAbort, PidList});
        abort   -> throw({aborted, PidList});
        _Else   -> true
    end,
    doCommit(Name, Routing, TempRouting, SeqNum, PidList).

% (after receiving doCommit) propagate doCommit messages across the network and commit the change
doCommit(Name, Routing, TempRouting, SeqNum, PidList) ->
    % Propagate the doCommit message across the network
    propagate(PidList, fun(Dest) -> Dest ! {doCommit, self(), SeqNum} end),
    ets:delete(Routing),
    cleanQueue(Name, TempRouting, SeqNum).

% If this is the root router, don't wait for a doAbort message
waitAbort(Name, Routing, TempRouting, Pid, Pid, SeqNum, PidList) ->
    Pid ! {abort, self(), SeqNum},
    doAbort(Name, Routing, TempRouting, Pid, SeqNum, PidList);

% wait for a doAbort message then initiate the abort
waitAbort(Name, Routing, TempRouting, From, _, SeqNum, PidList) ->
    case request(From, From, SeqNum, {abort, self(), SeqNum}) of
        doAbort -> true;
        _Else   -> io:format("Bad abort. ~w ~n", [self()])
    end,
    doAbort(Name, Routing, TempRouting, From, SeqNum, PidList).

% (After receiving a doAbort message) propagate the doAbort message to neighbours and abort
doAbort(Name, Routing, TempRouting, From, SeqNum, PidList)  -> 
    % Propagate a doAbort message to all children
    propagate(PidList, fun(Dest) -> request(From, Dest, SeqNum, {doAbort, self(), SeqNum}) end),
    ets:delete(TempRouting),
    cleanQueue(Name, Routing, SeqNum).

% If the PidList is empty, just return
propagate([], _) -> true;
% Invokes MsgFunc() on all PIDs in PidList
propagate(PidList, MsgFunc) ->
    lists:foldl(fun(Pid, _) -> MsgFunc(Pid) end, notused, PidList).

% Otherwise
request(From, Dest, SeqNum, Message) ->
    % io:format("DEBUG: ~w Entering commitloop ~w ~n", [self(), Message]),
    Dest ! Message,
    Result = commitloop(Dest, From, SeqNum),
    % io:format("DEBUG: Loop exited: ~w ~n", [Result]),
    Result.

% Remove any messages pertaining to the recently completed 2PC out of the message queue
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

% Name:     the name of the router
% Routing:  the routing table for the router
loop(Name, Routing) ->
    receive
        {message, Dest, _, Pid, Trace} ->
            message(Name, Routing, Dest, Pid, Trace);
        {control, From, Pid, SeqNum, ControlFun} ->
            % io:format("DEBUG: ~w received control message ~n", [self()]),
            control(Name, Routing, From, Pid, SeqNum, ControlFun);
        {dump, From} -> From ! {table, self(), ets:match(Routing, '$1')};
        stop         -> stop(Routing)
    end,

    loop(Name, Routing).