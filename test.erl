-module(test).
-export([runTest/0]).

simpleNetworkGraph() ->
  [{red,   [{white, [white, green]}, {blue, [blue]}]},
   {white, [{red, [blue]}, {blue, [green, red]}]},
   {blue , [{green, [white, green, red]}]},
   {green, [{red, [red, blue, white]}]}
  ].

runTest() ->
    io:format("Running test ~n"),
    Graph = simpleNetworkGraph(),
    Pid = control:graphToNetwork(Graph),
    io:format("Pid = ~w ~n", [Pid]).