-module(router).
-export([start/1]).

% 
start(RouterName) ->
    % Start the process
    Pid = spawn(fun() -> loop(RouterName, 
                ets:new(routing, [private,set]), []) end),
    Pid.

% 
stop(Routing) ->
    % Message all nodes to remove this node
    ets:foldl(fun({_, Pid}, DontCare) -> 
        Pid ! stop, 
        DontCare
    end, notused, Routing),

    ets:delete(Routing),
    exit(normal).

%
message(Name, Routing, Dest, Pid, Trace) ->
    % Determine if this router is the destination
    if
        % If it is, send back an acknowledgement message to the sender
        Name == Dest -> Pid ! {trace, self(), lists:append(Trace, [Name])};
        % Otherwise, append this router to the trace and pass the message on
        true ->
            % First, make sure the destination router can be reached
            case ets:lookup(Routing, Dest) of
                [{_, Next}] -> Next ! {message, Dest, self(), Pid, lists:append(Trace, [Name])};
                true        -> Pid ! {error, "Invalid message forward request"}
            end
    end.

%
control(Name, Routing, FuncList, From, Pid, SeqNum, ControlFun) ->
    % Complete the control request if it has not already been completed
    case list:member(SeqNum, FuncList) of
        true  -> Children = none;
        false ->
            TempRouting = ets:new(temp, [private,set]),
            Children = ControlFun(Name, TempRouting),
            lists:append(FuncList, [SeqNum]),

            % If the control request is not an initialization, propagate it
            case SeqNum of
                0    -> true;
                true -> Children = propagate(Children, Routing, 
                                             Pid, SeqNum, ControlFun)
            end
    end,

    % Send a response to the node that sent the message
    case Children of
        none  ->
            From ! {done, self(), SeqNum},
            Routing;
        abort ->
            % Send the abort message to all children
            From ! {abort, self(), SeqNum},
            ets:delete(TempRouting),
            Routing;
        _Else -> 
            ets:delete(Routing),
            From ! {commited, self(), SeqNum},
            TempRouting
    end.

%
propagate(Routing, Pid, SeqNum, ControlFun) ->
    ets:foldl(fun({_, RPid}, DontCare) ->
        RPid ! {control, self(), Pid, SeqNum, ControlFun},
        % Wait for a response
        receive
            {committed, RPid, SeqNum} -> committed;
            {abort, RPid, SeqNum}     -> abort;
            {done, RPid, SeqNum}      -> committed
        end
    end, notused, Routing).

% Name:     the name of the router
% Routing:  the routing table for the router
% FuncList  a list of all SeqNum's that have been performed
loop(Name, Routing, FuncList) ->
    receive
        {message, Dest, From, Pid, Trace} ->
            message(Name, Routing, Dest, Pid, Trace),
            From;
        {control, From, Pid, SeqNum, ControlFun} ->
            Routing = control(Name, Routing, FuncList, From, Pid, SeqNum, ControlFun),
        {dump, From} -> From ! {table, self(), ets:match(Routing, '$1')};
        stop         -> stop(Routing)
    end,

    loop(Name, Routing, FuncList).