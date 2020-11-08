-module(control).
-export([graphToNetwork/1]).

graphToNetwork(Graph) ->
    ets:new(nodes, [named_table, set]),
    Pid = graphInit(Graph, 0),
    ets:delete(nodes),

    Pid.

% Start all of the router processes
graphInit(Graph, SeqNum) ->
    {Name, Edges} = lists:nth(1, Graph),
    Pid = router:start(Name),

    % Add the router to the nodes table
    ets:insert(nodes, {Name, Pid}),

    if
        % if the list has more elements, add them
        length(Graph) > 1 -> graphInit(lists:nthtail(1, Graph), SeqNum);
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
    Pid ! {control, self(), self(), SeqNum,
        fun(_, Table) -> ets:insert(Table, List), []
    end},

    Pid.

%
% extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) ->
%     Pid = graphInit([{NodeName, Edges}], SeqNum),
    
%     % Send the rootpid the request to include the node
%     RootPid ! {control, self(), self(), SeqNum, fun(_) ->
%         ___ end},

%     RootPid,
%     SeqNum,
%     From,
%     NodeName,
%     Edges.
