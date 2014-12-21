1. Install Erlang runtime - https://www.erlang-solutions.com/downloads/download-erlang-otp 
Verify the installation :
Check that erl has found the crypto module ok
$ erl
Erlang R16B01 (erts-5.10.2)  [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]
Eshell V5.10.2 (abort with ^G)

1> crypto:start().
ok
2>
User switch command
--> q

2. Install Eclipse and erlide (Erlang IDE) by following these steps http://erlide.org/installation/  You can read about it here- https://github.com/erlide/erlide/wiki 

3. Hello World - https://github.com/erlide/erlide/wiki/Tutorial-Hello-World The hello_world.erl file must look like this else you will have some compilation errors:

4. The hello_world folder contains Erlang source files which demonstrate the use of the language.
You can use Eclipse with the Erlang language support. The contents of hello_world are:

1. hello_world.erl - This program just prints "Hello World" to the output.

2. hello_world_actor.erl - This program shows a receive loop which only breaks if the input is "goobye" (without the quotes)

3. echo.erl - This  program just echoes the input.

4. server.erl - This program demonstrates the Process pattern - tail recursive state handler

5. frequency.erl - This program demonstrates the client-server pattern.
