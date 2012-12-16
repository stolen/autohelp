Erlang autohelp `parse_transform`
=================================

This `parse_transform` makes your code self-descriptive.
When you add `@doc` annotation for module or function, it goes to EDoc's output. With `autohelp` you can view it using `Module:help` functions:

 * `Module:help()` shows module description and list of functions with names of arguments
 * `Module:help(FunctionName)` shows brief description for functions with given name and all arities
 * `Module:help(FinctionName, Arity)` shows full function description

All output goes to `stdout` (it is designed to look well in shell), return values are `ok`.
