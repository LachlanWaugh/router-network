-module(control).
-export([graphToNetwork/1]).

graphToNetwork(Graph) ->
    ets:new(nodes, [named_table, set]),
    Pid = graphInit(Graph),

    sendMessage(),

    deleteall(),
    ets:delete(nodes),
    Pid.

deleteall() ->
    ets:foldl(fun({_, Pid}, DontCare) -> 
            Pid ! stop,
            DontCare
        end, notused, nodes).

sendMessage() ->
    [{_, RedPid}] = ets:lookup(nodes, red),

    RedPid ! {message, blue, self(), self(), []},

    receive
        {trace, Pid, Trace} -> io:format("~w ~p ~n", [Pid, Trace])
    end.

% Start all of the router processes
graphInit(Graph) ->
    {Name, Edges} = lists:nth(1, Graph),
    Pid = router:start(Name),

    % Add the router to the nodes table
    ets:insert(nodes, {Name, Pid}),

    if
        % if the list has more elements, add them
        length(Graph) > 1 -> graphInit(lists:nthtail(1, Graph));
        true ->              Pid
    end,

    % Convert the list of edges into the correct format for the routing table
    List = lists:flatten(lists:map(fun(E) ->
            {Route, Dest} = E,
            % Replace the name of the routing node with it's PID
            [{_, RPid}] = ets:lookup(nodes, Route),
            % Convert {white, [white, green]} -> [{white, whitepid}, {green, whitepid}]
            lists:zip(Dest, lists:duplicate(length(Dest), RPid))
        end, Edges)),

    % Send the routing table to the router
    Pid ! {control, self(), self(), 0, fun(Table) -> 
            ets:insert(Table, List),
            []
        end},

    Pid.

% extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) -> extendNetwork/4