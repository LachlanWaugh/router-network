-module(controlTest).
-export([runTest/0]).

%% Circular network consisting of three nodes.
%%
circularNetwork3 () ->
  [{red  , [{white, [white, blue]}]},
   {white, [{blue , [red, blue]}]},
   {blue , [{red  , [red, white]}]}
  ].

%% Reverse circle as in circularNetwork3/0
%%
reverseCircularNetwork3 () ->
  [{red  , [{blue , [white, blue]}]},
   {white, [{red  , [red, blue]}]},
   {blue , [{white, [red, white]}]}
  ].

%% Build a circular network and then use a control request to change the 
%% direction of all edges; verify the original and reversed network
%%
runTest () ->
  io:format ("*** Starting router network...~n"),
  CGraph = circularNetwork3 (),
  RedPid = control:graphToNetwork (CGraph),
  networkTest:verifyNetwork (RedPid, CGraph),

  {WhitePid, _} = networkTest:probeNetwork (RedPid, white),
  {BluePid , _} = networkTest:probeNetwork (RedPid, blue ),
  if (WhitePid == undef) or (BluePid == undef) -> 
      io:format ("*** ERROR: Corrupt network!~n");
     true -> true
  end,

  io:format ("*** Reversing routing cycle...~n"),
  RedPid ! {control, self (), self (), 1, 
	    fun (Name, Table) -> 
		case Name of
		  red   -> ets:insert (Table, [{white, BluePid }, 
					       {blue , BluePid }]);
		  white -> ets:insert (Table, [{red  , RedPid  }, 
					       {blue , RedPid  }]);
		  blue  -> ets:insert (Table, [{red  , WhitePid}, 
					       {white, WhitePid}])
		end,
		[]
	    end},
  receive
    {committed, RedPid, 1} -> io:format ("*** ...done.~n");
    {abort    , RedPid, 1} -> 
      io:format ("*** ERROR: Re-configuration failed!~n")
  after 10000              ->
      io:format ("*** ERROR: Re-configuration timed out!~n")
  end,
  networkTest:verifyNetwork (RedPid, reverseCircularNetwork3 ()).