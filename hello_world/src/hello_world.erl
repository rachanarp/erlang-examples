%%file_comment

-module(hello_world).

%% ====================================================================
%% API functions
%% ====================================================================
-export([say_hello/0]).
say_hello() -> io:fwrite("hello, world\n").


 
%% ====================================================================
%% Internal functions
%% ====================================================================

