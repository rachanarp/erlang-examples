%%Example : client-server pattern. Call frequency:start().
%% Call frequency:allocate() 6 times to loop through all 6 frequencies.
%% Call frequency:deallocate(10) to deallocate the frequency number 10.
%% Call frequency:allocate() to note that the newly deallocated frequency 10 is now allocated
%% Call frequency:stop() to stop the process

-module(frequency).

%% ====================================================================
%% API functions
%% ====================================================================
-export([deallocate/1, allocate/0, stop/0, start/0]).
-export([init/0]).

%% Start functions - create and initialize the server

start() ->
	register(frequency, spawn(frequency,init,[])).

init() ->
	Frequencies = {get_frequencies(),[]},
	loop(Frequencies).
	

get_frequencies() -> [10,11,12,13,14,15].


loop(Frequencies) ->
	receive
		{request,Pid, allocate} -> 
			{NewFrequencies, Reply} = allocate(Frequencies,Pid), 
			 reply(Pid, Reply),
			 loop(NewFrequencies);
		{request,Pid, {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq),
			reply(Pid, ok),
			loop(NewFrequencies);
		{request,Pid,stop} ->
			reply(Pid, ok)
	end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).


%% information hiding using call
call(Message) ->
	frequency ! {request, self(), Message},
	receive
		{reply, Reply} -> Reply
	end.

	

%% ====================================================================
%% Internal functions
%% ====================================================================
allocate({[],Allocated}, _Pid) ->
{{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
{{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated=lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free], NewAllocated}.

