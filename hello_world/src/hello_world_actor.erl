%%file_comment

-module(hello_world_actor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, loop/0]).

start() -> spawn(fun() -> loop() end).

%% ====================================================================
%% Internal functions
%% ====================================================================

loop() ->
	receive
		hello ->
			io:format("Hello, World |~n"),
			loop();
		
		goobye ->
			ok
	end.

