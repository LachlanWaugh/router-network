-module(testAbort).
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

    Pid ! {control, self(), self(), 1, fun(Name, _) -> 
        case Name of
            white -> abort;
            _Else -> []
        end
    end},

    io:format("~w ~n", [Pid]),

    receive
        {committed, Pid, 1} -> io:format("Test failed~n");
        {abort, Pid, 1}     -> io:format("Test passed~n")
    end.

% c(control), c(router), c(test), test:runTest().
% c(control), c(router), c(testAbort), testAbort:runTest().

% c(control), c(router), c(networkTest), networkTest:runTest().
% c(control), c(router), c(networkTest), c(controlTest), controlTest:runTest().
% c(control), c(router), c(networkTest), c(controlTest), c(extendTest), extendTest:runTest(). 