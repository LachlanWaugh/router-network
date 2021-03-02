-module(extendTest).
-export([runTest/0]).

%% Simple router network consisting of four nodes
%%
simpleNetworkGraph () ->
  [{red  , [{white, [white, green]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]}]},
   {blue , [{green, [white, green, red]}]},
   {green, [{red, [red, blue, white]}]}
  ].

%% Extended router network consisting of five nodes
%%
extendedNetworkGraph () ->
  [{red  , [{white, [white, green, black]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]},
	    {black, [black]}]},
   {blue , [{green, [white, green, red, black]}]},
   {green, [{red, [red, blue, white, black]}]},
   {black, [{red, [red, green]},
	    {green, [white, blue]}]}
  ].

%% Extend the simple network graph by one new node and verify the
%% resulting network against a graph specification
%%
runTest () ->
  io:format ("*** Starting router network...~n"),
  Graph = simpleNetworkGraph (),
  RedPid = control:graphToNetwork (Graph),
  networkTest:verifyNetwork (RedPid, Graph),

  {WhitePid, _} = networkTest:probeNetwork (RedPid, white),
  {BluePid , _} = networkTest:probeNetwork (RedPid, blue ),
  {GreenPid, _} = networkTest:probeNetwork (RedPid, green),
  if (WhitePid == undef) or (BluePid == undef) or (GreenPid == undef) -> 
      io:format ("*** ERROR: Corrupt network!~n");
     true -> true
  end,

  io:format ("*** Extending network...~n"),
  case control:extendNetwork (RedPid, 1, white, 
			      {black, [{RedPid  , [red, green]}, 
				       {GreenPid, [white, blue]}
				      ]})
    of
    true  -> io:format ("*** ...done.~n");
    false -> io:format ("*** ERROR: Extension failed!~n")
  end,
  networkTest:verifyNetwork (RedPid, extendedNetworkGraph ()).