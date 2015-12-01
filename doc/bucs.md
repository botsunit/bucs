

# Module bucs #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compare_as_atom-2">compare_as_atom/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare_as_binary-2">compare_as_binary/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare_as_integer-2">compare_as_integer/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare_as_list-2">compare_as_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare_as_string-2">compare_as_string/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_string-1">is_string/1</a></td><td>
Check if the given value is a string.</td></tr><tr><td valign="top"><a href="#module_exist-1">module_exist/1</a></td><td>
Check if the given module exist.</td></tr><tr><td valign="top"><a href="#pipecall-1">pipecall/1</a></td><td> 
Pipe fun call.</td></tr><tr><td valign="top"><a href="#to_atom-1">to_atom/1</a></td><td>
Convert the given term to atom.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>
Convert the given term to binary.</td></tr><tr><td valign="top"><a href="#to_float-1">to_float/1</a></td><td>
Convert the given term to float.</td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1</a></td><td>
Convert the given term to integer.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>
Convert the given term to list.</td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>
Convert the given term to string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compare_as_atom-2"></a>

### compare_as_atom/2 ###

`compare_as_atom(V1, V2) -> any()`

<a name="compare_as_binary-2"></a>

### compare_as_binary/2 ###

`compare_as_binary(V1, V2) -> any()`

<a name="compare_as_integer-2"></a>

### compare_as_integer/2 ###

`compare_as_integer(V1, V2) -> any()`

<a name="compare_as_list-2"></a>

### compare_as_list/2 ###

`compare_as_list(V1, V2) -> any()`

<a name="compare_as_string-2"></a>

### compare_as_string/2 ###

`compare_as_string(V1, V2) -> any()`

<a name="is_string-1"></a>

### is_string/1 ###

`is_string(V) -> any()`

Check if the given value is a string

<a name="module_exist-1"></a>

### module_exist/1 ###

`module_exist(Module) -> any()`

Check if the given module exist

<a name="pipecall-1"></a>

### pipecall/1 ###

`pipecall(Rest) -> any()`


Pipe fun call

Example:

```

 Add = math:pow(7, 3),
 Log = math:log(Add),
 Mul = multiplication(Log, 7),
 Res = addition(Mul, 7).

 % With bucs:pipecall/2 :
 Res = bucs:pipecall([
                      {fun math:pow/2, [7, 3]},
                      fun math:log/1,
                      {fun multiplication/2, [7]},
                      {fun addition/2, [7]}
                     ])).
```

<a name="to_atom-1"></a>

### to_atom/1 ###

`to_atom(X) -> any()`

Convert the given term to atom

Example:

```

  atom = bucs:to_atom(atom).
  atom = bucs:to_atom(<<"atom">>).
  atom = bucs:to_atom("atom").
```

<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(V) -> any()`

Convert the given term to binary

Example:

```
<<"list">> = bucs:to_binary(list).<<"list">> = bucs:to_binary("list").<<"list">> = bucs:to_binary(<<"list">>).<<"123">> = bucs:to_binary(123).<<"1.20000000000000000000e+01">> = bucs:to_binary(12.0).<<"true">> = bucs:to_binary(true).<<"false">> = bucs:to_binary(false).
```

<a name="to_float-1"></a>

### to_float/1 ###

`to_float(Value) -> any()`

Convert the given term to float

Example

```

  123.45 = bucs:to_float(123.45).
  123.45 = bucs:to_float("123.45").
  123.45 = bucs:to_float(<<"123.45">>).
  123.45 = bucs:to_float('123.45').
  123.0 = bucs:to_float(123).
```

<a name="to_integer-1"></a>

### to_integer/1 ###

`to_integer(I) -> any()`

Convert the given term to integer

Example

```

  123 = bucs:to_integer(123).
  123 = bucs:to_integer("123").
  123 = bucs:to_integer(<<"123">>).
  123 = bucs:to_integer('123').
  123 = bucs:to_integer(123.456).
```

<a name="to_list-1"></a>

### to_list/1 ###

`to_list(V) -> any()`

Convert the given term to list

Example:

```

  "list" = bucs:to_list(list).
  "list" = bucs:to_list("list").
  "list" = bucs:to_list(<<"list">>).
  "123" = bucs:to_list(123).
  "1.20000000000000000000e+01" = bucs:to_list(12.0).
  "true" = bucs:to_list(true).
  "false" = bucs:to_list(false).
```

<a name="to_string-1"></a>

### to_string/1 ###

`to_string(V) -> any()`

Convert the given term to string

