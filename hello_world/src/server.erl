%%Process pattern - tail recursive state handler

-module(server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([call/2, stop/1, start/2]). 
-export([init/1]).

start(Name, Data) ->
	Pid = spawn(generic_handler, init, [Data]),
	register(Name, Pid),
	ok.

stop(Name) ->
	Name ! {stop, self()},
	receive 
		{reply, Reply} -> Reply
	end.

call(Name, Msg) ->
	Name ! {request, self(), Msg},
	receive 
		{reply, Reply} -> Reply
	end.

reply(To, Msg) ->
	To ! {reply, Msg}.

init(Data) ->
	loop(initialize(Data)).

loop(State) ->
	receive
		{request, From, Msg} ->
			{Reply, NewState} = handle_msg(Msg, State),
			reply(From, Reply),
			loop(NewState);
		
		{stop, From} ->
			reply(From, terminate(State))
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

initialize(Data) ->
	io:format("Initialized").

handle_msg(Msg, State) ->
	io:format("Handling message - ~w~n", Msg).

terminate(State) ->
  io:format("Terminate state - ~w~n", State).
