-module(control).
-export([graphToNetwork/1, extendNetwork/4]).

graphToNetwork(Graph) ->
    ets:new(nodes, [named_table, set]),
    Pid = startRouter(init, Graph),
    ets:delete(nodes),
    Pid.

% 
startRouter(extend, {Name, Edges}) ->
    Pid = router:start(Name),
    sendEdges(Pid, Edges, fun(X) -> X end);

% Start all of the router processes
startRouter(init, Graph) ->
    {Name, Edges} = lists:nth(1, Graph),
    Pid = router:start(Name),
    % Add the router to the nodes table
    ets:insert(nodes, {Name, Pid}),

    if
        % if the list has more elements, add them
        length(Graph) > 1 -> startRouter(init, lists:nthtail(1, Graph));
        true ->              true
    end,

    sendEdges(Pid, Edges, fun(X) -> 
        [{_, RPid}] = ets:lookup(nodes, X),
        RPid
    end).

% 
sendEdges(Pid, Edges, PidFunc) ->
    % Convert the list of edges into the correct format for the routing table
    List = lists:flatten(lists:map(fun({Route, Dest}) ->
        % Replace the name of the routing node with it's PID
        RPid = PidFunc(Route),
        % Convert {white, [white, green]} -> [{white, whitepid}, {green, whitepid}]
        lists:zip(Dest, lists:duplicate(length(Dest), RPid))
    end, Edges)),

    % Send the routing table to the router
    Pid ! {control, self(), self(), 0, 
        fun(_, Table) -> ets:insert(Table, List), [] end},

    Pid.

% 
extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) ->
    Pid = startRouter(extend, {NodeName, Edges}),

    % Send the rootpid the request to include the node
    RootPid ! {control, self(), self(), SeqNum, 
        fun(Name, Table) ->
            case Name of
                NodeName -> true;
                From     -> ets:insert(Table, {NodeName, Pid});
                % Route through From
                _Else    ->
                    [{_, Node}] = ets:lookup(Table, From),
                    ets:insert(Table, {NodeName, Node})
            end
    end},

    receive
        {committed, _, _} -> true;
        {abort, _, _}     -> false
    end.