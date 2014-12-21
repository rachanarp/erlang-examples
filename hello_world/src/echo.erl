%% file_comment

-module(echo).

%% ====================================================================
%% API functions
%% ====================================================================
-export([loop/0, go/0]).

go() ->
	%%Pid = spawn(echo,loop, []),
	%%Pid ! {self(), hello},

	register(echo, spawn(echo, loop, [])),
	echo ! {self(), hello},
	receive
		{_Pid, Msg} -> 
			io:format("~w~n", [Msg])
	end.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


