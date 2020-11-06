-module(router).
-export([start/1]).

start(RouterName) ->
    % Start the process
    Pid = spawn(fun() -> loop(RouterName, ets:new(routing, [private,set])) end),
    io:format("~w: ~w ~n", [RouterName, Pid]),
    Pid.

stop(Routing) ->
    ets:delete(Routing),
    exit(normal).

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

control(Routing, Pid, SeqNum, ControlFun) ->
    case SeqNum of
        % Initialization
        0    -> Children = ControlFun(Routing);
        true -> Children = abort
    end,

    case Children of
        abort -> Pid ! {abort, self(), SeqNum};
        _Else -> Pid ! {commited, self(), SeqNum}
    end,
    Children.

loop(Name, Routing) ->
    receive
        {message, Dest, From, Pid, Trace} ->
            io:format("~w received message from ~w ~n", [Name, From]),
            message(Name, Routing, Dest, Pid, Trace),
            From;
        {control, From, Pid, SeqNum, ControlFun} ->
            control(Routing, Pid, SeqNum, ControlFun),
            From;
        {dump, From} -> From ! {table, self(), ets:match(Routing, '$1')};
        stop         -> stop(Routing)
    end,

    loop(Name, Routing).

% c(control), c(router), c(test), test:runTest().