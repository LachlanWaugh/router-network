-module(control).
-export([graphToNetwork/1, extendNetwork/4]).

graphToNetwork(Graph) ->
    First_pid = router:start()

extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) ->