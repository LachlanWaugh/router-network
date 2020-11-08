-module(router).
-export([start/1]).

%
start(RouterName) ->
    % Start the process
    Pid = spawn(fun() -> loop(RouterName, ets:new(routing, [private,set])) end),
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
        abort -> Pid ! {abort, self(), 0};
        _Else -> Pid ! {committed, self(), 0}
    end;
% Otherwise, handle a normal control request
control(Name, Routing, From, Pid, SeqNum, ControlFun) ->
    TempRouting = ets:new(temp, [private,set]),
    Children = ControlFun(Name, TempRouting),

    % Check whether the control function succeeded
    case Children of
        abort -> doAbort(Name, Routing, TempRouting, From, SeqNum, PidList);
        _Else -> false
    end,

    % Attempt to propagate the message across the network
    try
        PidList = checkNeighbours(Routing, From, Pid, SeqNum, ControlFun),
        doCommit(Name, Routing, TempRouting, From, SeqNum, PidList)
    catch
        throw:{abort,   PidList} -> doAbort(Name, Routing, TempRouting, From, SeqNum, PidList);
        throw:{aborted, PidList} -> 
    end.

% Propagate a message to all children
checkNeighbours(Routing, From, Pid, SeqNum, ControlFun) ->
    PidList = [],
    % Find unique PIDs from the Routing Table (direct edges)
    ets:foldl(fun({_, Dest}, _) -> 
        case lists:member(Dest, PidList) of
            false -> lists:append(PidList, [Dest]);
            true  -> true
        end
    end, notused, Routing),

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
doCommit(Name, Routing, TempRouting, From, SeqNum, PidList) ->
    % Wait for a doCommit or doAbort message
    case request(From, From, SeqNum, {canCommit, self(), SeqNum}) of
        aborted -> throw({aborted, PidList});
        abort   -> throw({aborted, PidList});
        _Else   -> true
    end

    % Propagate the doCommit message across the network
    propagate(PidList, fun(Dest) -> request(From, Dest, {doCommit, self(), SeqNum}) end),

    ets:delete(Routing),
    loop(Name, TempRouting).

%
doAbort(Name, Routing, TempRouting, From, SeqNum, PidList)  -> 
    % Wait for the doAbort message
    case request(From, From, SeqNum, {abort, self(), SeqNum}) of
        aborted -> true;
        _Else   -> io:format("Bad abort. ~n")
    end,

    % Propagate a doAbort message to all children
    propagate(PidList, fun(Dest) -> request(From, Dest, {doAbort, self(), SeqNum}) end),

    ets:delete(TempRouting),
    loop(Name, Routing).

% Invokes MsgFunc() on all nodes directly connected
propagate(PidList, MsgFunc) ->
    list:foldl(fun({_, Dest}, _) -> MsgFunc(Dest) end, notused, PidList).

% Otherwise
request(From, Dest, SeqNum, Message) ->
    Dest ! Message,
    Result = commitloop(Dest, From, SeqNum),
    Result.
%
commitloop(Dest, From, SeqNum) ->
    receive
        % If you get another control request of the same type, ignore it
        {control, FromPid, _, SeqNum, _} -> 
            FromPid ! {canCommit, self(), SeqNum},
            commitloop(Dest, SeqNum);
        % If you get a control request of a different number, abort both
        {control, FromPid, _, OtherNum, _} -> 
            FromPid ! {abort, self(), OtherNum},
            abort;
        {canCommit, Dest, SeqNum} -> canCommit;
        {abort,     Dest, SeqNum} -> abort;
        {doCommit,  From, SeqNum} -> committed;
        {doAbort,   From, SeqNum} -> aborted;
    end.

% Name:     the name of the router
% Routing:  the routing table for the router
loop(Name, Routing) ->
    receive
        {message, Dest, _, Pid, Trace} ->
            message(Name, Routing, Dest, Pid, Trace);
        {control, From, Pid, SeqNum, ControlFun} ->
            control(Name, Routing, From, Pid, SeqNum, ControlFun);
        {dump, From} -> From ! {table, self(), ets:match(Routing, '$1')};
        stop         -> stop(Routing)
    end,

    loop(Name, Routing).