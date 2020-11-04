-module(router).
-export([start/1]).

start(RouterName) ->
    Pid = spawn(?MODULE, loop, [RouterName, ets:new()]),
    register(RouterName, Pid),
    Pid.

stop(Table) ->
    delete(Table),
    exit(normal).

loop(Name, Table) ->
    receive
        {message, Dest, From, Pid, Trace} ->
            % Determine if this router is the destination
            if
                % If it is, send back an acknowledgement message to the sender
                Name == Dest ->
                    Pid ! {trace, self(), lists:append(Trace, Name)};
                % Otherwise, append this router to the trace and pass the message on
                true ->
                    % First, make sure the destination router can be reached
                    case ets:lookup(Table, Dest) of
                        [{_, Next}] ->
                            Next ! {message, Dest, self(), Pid, lists:append(Trace, Name)};
                        _Else ->
                            Pid ! {error, "Invalid message forward request"},
                    end,
            end,
            loop(Table);
        {control, From, Pid, SeqNum, ControlFun} ->
            loop(Table);
        {dump, From} ->
            From ! {table, self(), ets:match(Table, '$1')},
            loop(Table);
        stop ->
            stop(Table);
    end.