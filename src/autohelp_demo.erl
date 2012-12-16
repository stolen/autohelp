% @doc autohelp_demo: example of autohelp use
% autohelp_demo:help() will show module summary (this note and function list)
% autohelp_demo:help(Function) will show brief description for functions
% of given name and all arities
% autohelp_demo:help(Function, Arity) will show full description for function.
-module(autohelp_demo).
-compile({parse_transform, autohelp}).

-export([hello/0, hello/1, hello/2, nodoc/0]).

% @doc Sample function of zero arity.
% Just prints "hello". This is
% multi-line documentation for it.
hello() ->
  % This comment is ignored
  io:format("Hello").

% @doc Another sample function with arity 1.
% help(hello) should print one-line summary for
% all functions named 'hello' (all arities).
hello(Name) ->
  io:format("Hello, ~p~n", [Name]).

hello(Name, Times) ->
  [hello(Name) || _ <- lists:seq(1, Times)],
  ok.

% exported function without doc
nodoc() ->
  internal().

% @doc This function is internal (not exported)
% edoc does not describe such functions, so we don't either
internal() ->
  internal_nodoc().

% Obviously, internal undocumented functions should not appear in autohelp
internal_nodoc() ->
  ok.
