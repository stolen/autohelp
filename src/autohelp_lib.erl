-module(autohelp_lib).
-export([pad_text_left/2, help/1, help/2]).

pad_text_left(Text, PadWidth) ->
  Padding = lists:duplicate(PadWidth, $ ),
  re:replace(Text, "(^|\n)", "\\1" ++ Padding, [global]).


% Internal function: name and return format is similar to auto-added by parse_transform
'__autohelp_fun_desc'() ->
  [
    {some_fun, 1, "some_fun(SomeArg)", "ShortDesc/1", "Full Description/1\nMulti-line :)"},
    {some_fun, 2, "some_fun(SomeArg, SecondArg)", "ShortDesc/2", "Full Description/2\nMulti-line too :)"},
    {other_fun, 0, "other_fun()", "OtherDesc", "Description for Other function"}
  ].
% Proxy for pad_text_left, named as auto-added one
'__autohelp_pad_text_left'(Text, PadWidth) ->
  pad_text_left(Text, PadWidth).

help(FunName) ->
  [io:format("    ~s~n  ~s~n", [Header, '__autohelp_pad_text_left'(BriefDesc, 10)]) ||
    {DescName, _Ar, Header, BriefDesc, _FullDesc} <- '__autohelp_fun_desc'(),
    DescName == FunName],
  ok.

help(FunName, Arity) ->
  [io:format("    ~s~n  ~s~n", [Header, '__autohelp_pad_text_left'(FullDesc, 10)]) ||
    {DescName, DescArity, Header, _BriefDesc, FullDesc} <- '__autohelp_fun_desc'(),
    DescName == FunName,
    DescArity == Arity],
  ok.
