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

% c(control), c(router), c(test), test:runTest().
% c(control), c(router), c(networkTest), networkTest:runTest().
% c(control), c(router), c(networkTest), c(controlTest), controlTest:runTest().