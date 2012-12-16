-module(autohelp).
-export([parse_transform/2]).

-include_lib("xmerl/include/xmerl.hrl").

parse_transform(AST, _Options) ->
  [File|_] = [FileName || {attribute, _, file, {FileName, _}} <- AST],
  {Module, DocXML} = edoc:get_doc(File),
  case DocXML of
    #xmlElement{name = module} ->
      add_doc_from_xml(AST, Module, DocXML);
    _ ->
      io:format("~s: cannot retrieve documentation for module ~s~n", [?MODULE, Module])
  end.

add_doc_from_xml(AST, Module, #xmlElement{} = DocXML) ->
  HelpExports = {attribute, 0, export, [{help, 0}, {help, 1}, {help, 2}]},

  {_, ModFullDesc} = get_descriptions(DocXML),
  FunctionDescriptions = describe_functions(get_child(functions, DocXML)),

  % Helpers
  FunctionDescriptionsFun = make_returner('__autohelp_fun_desc', FunctionDescriptions),
  TextPadder = copy_lib_function(pad_text_left, 2, '__autohelp_pad_text_left'),

  ModHelp = make_module_help(Module, ModFullDesc, FunctionDescriptions),
  FunHelp1 = copy_lib_function(help, 1, help),
  FunHelp2 = copy_lib_function(help, 2, help),

  AddedFunctions = [FunctionDescriptionsFun, TextPadder, ModHelp, FunHelp1, FunHelp2],
  Result = insert_before_functions([HelpExports | AddedFunctions], AST),
  Result.


get_children(Name, #xmlElement{content = Content}) ->
  [Child || #xmlElement{name = Name_} = Child <- Content, Name_ == Name].

get_child(Name, #xmlElement{content = Content}) ->
  lists:keyfind(Name, #xmlElement.name, Content).

% get_attrs(Name, #xmlElement{attributes = Attrs}) ->
%   [Attr || #xmlAttribute{name = Name_} = Attr <- Attrs, Name_ == Name].

get_attr(Name, #xmlElement{attributes = Attrs}) ->
  lists:keyfind(Name, #xmlAttribute.name, Attrs).

value(#xmlAttribute{value = Value}) ->
  Value;
value(_) ->
  undefined.


get_descriptions(#xmlElement{} = E) ->
  case get_child(description, E) of
    false ->
      {"", ""};
    #xmlElement{} = Description ->
      Brief = get_child(briefDescription, Description),
      Full = get_child(fullDescription, Description),
      {get_text(Brief), get_text(Full)}
  end.

get_text(#xmlElement{content = [#xmlText{value = Text}]}) ->
  Text;
get_text(_) ->
  "".



describe_functions(#xmlElement{} = XML) ->
  [fun_desc_from_xml(Function) || Function <- get_children(function, XML),
    value(get_attr(exported, Function)) == "yes"].

fun_desc_from_xml(#xmlElement{} = FunXML) ->
  Name = list_to_atom(value(get_attr(name, FunXML))),
  Arity = list_to_integer(value(get_attr(arity, FunXML))),

  ArgsXML = get_child(args, FunXML),
  ArgNames = [get_text(get_child(argName, ArgXML)) || ArgXML <- get_children(arg, ArgsXML)],

  Header = lists:flatten(io_lib:format("~s(~s)", [Name, string:join(ArgNames, ", ")])),

  {BriefDesc, FullDesc} = get_descriptions(FunXML),
  {Name, Arity, Header, BriefDesc, FullDesc}.



make_module_help(Module, ModFullDesc, FunDescriptions) ->
  PaddedDesc = autohelp_lib:pad_text_left(ModFullDesc, 3),
  FunHeaders = [["\n     ->  ", Header] || {_Fun, _Ar, Header, _bDesc, _fDesc} <- FunDescriptions],
  HelpFormat = "Module ~s~n----------------------------------~n   ~s~n----------------------------------~s~n",
  {function, 0, help, 0,
    [{clause, 0, [], [],
        [{call, 0, {remote, 0, {atom, 0, io}, {atom, 0, format}}, [
              {string, 0, HelpFormat},
              erl_parse:abstract([Module, PaddedDesc, FunHeaders], 0) ]}]
      }]
  }.


make_returner(ReturnerName, ReturnedValue) ->
  {function, 0, ReturnerName, 0,
    [{clause, 0, [], [],
        [erl_parse:abstract(ReturnedValue)]}]}.


copy_lib_function(SrcLibFunName, SrcArity, NewFunName) ->
  {ok, {_, [{abstract_code, {_, AST}}]}} = beam_lib:chunks(code:which(autohelp_lib), [abstract_code]),
  [FunBody] = [Body || {function, _, FunName, Arity, Body} <- AST,
    FunName == SrcLibFunName,
    Arity == SrcArity],
  {function, 0, NewFunName, SrcArity, FunBody}.

insert_before_functions(Items, AST) ->
  {Before, Functions} = lists:splitwith(fun(I) -> element(1, I) /= function end, AST),
  Before ++ Items ++ Functions.
