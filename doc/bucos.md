

# Module bucos #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-1">run/1</a></td><td>Equivalent to <a href="#run-2"><tt>run(Cmd, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td>
Execute the given shell command.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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
run(Cmd::string() | binary(), Timeout::integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Execute the given shell command

