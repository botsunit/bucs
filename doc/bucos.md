

# Module bucos #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = {timeout, integer()} | stdout_on_error | {return, combined | list, all | last | integer() | [integer()]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#in-2">in/2</a></td><td>Equivalent to <a href="#in-3"><tt>in(Path, Fun, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#in-3">in/3</a></td><td>
Execute the given function function in the given path.</td></tr><tr><td valign="top"><a href="#run-1">run/1</a></td><td>Equivalent to <a href="#run-2"><tt>run(Cmd, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td>
Execute the given shell command, waiting at most for a given timeout before returning
<tt>Options</tt> may contain:
<ul>
<li><tt>stdout_on_error</tt> : To get standard output in the result, in case of error.</li>
<li><tt>{timeout, integer()}</tt> : To set a maximum time to wait for, before returning with a <tt>{error,timeout}</tt> result.</li>
<li><tt>{return, list|combined, all|last|integer()|[integer()]}</tt> : To specify output collection</li>
</ul>
Note: If more than one shell commands are "chained" in the given string, only the first one is executed.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="in-2"></a>

### in/2 ###

`in(Path, Fun) -> any()`

Equivalent to [`in(Path, Fun, [])`](#in-3).

<a name="in-3"></a>

### in/3 ###

`in(Path, Fun, Args) -> any()`

Execute the given function function in the given path.

Example :

```erlang

  eos:in("/tmp", fun() ->
    ?assertMatch({ok, "/tmp"}, file:get_cwd())
    end).
```

<a name="run-1"></a>

### run/1 ###

<pre><code>
run(Cmd::string() | binary()) -&gt; term()
</code></pre>
<br />

Equivalent to [`run(Cmd, 5000)`](#run-2).

<a name="run-2"></a>

### run/2 ###

<pre><code>
run(Cmd::[string() | binary()], Timeout::integer() | [<a href="#type-options">options()</a>]) -&gt; {ok, string() | [string()]} | {error, integer()} | {error, integer(), string()}
</code></pre>
<br />

Execute the given shell command, waiting at most for a given timeout before returning
`Options` may contain:

* `stdout_on_error` : To get standard output in the result, in case of error.

* `{timeout, integer()}` : To set a maximum time to wait for, before returning with a `{error,timeout}` result.

* `{return, list|combined, all|last|integer()|[integer()]}` : To specify output collection


Note: If more than one shell commands are "chained" in the given string, only the first one is executed.

