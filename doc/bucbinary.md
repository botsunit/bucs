

# Module bucbinary #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#are_floats-1">are_floats/1</a></td><td></td></tr><tr><td valign="top"><a href="#are_integers-1">are_integers/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_float-1">is_float/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_integer-1">is_integer/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
join a list of binaries with the given separator.</td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="are_floats-1"></a>

### are_floats/1 ###

<pre><code>
are_floats(List::[binary()]) -&gt; true | false
</code></pre>
<br />

<a name="are_integers-1"></a>

### are_integers/1 ###

<pre><code>
are_integers(List::[binary()]) -&gt; true | false
</code></pre>
<br />

<a name="is_float-1"></a>

### is_float/1 ###

<pre><code>
is_float(Data::binary()) -&gt; true | false
</code></pre>
<br />

<a name="is_integer-1"></a>

### is_integer/1 ###

<pre><code>
is_integer(Data::binary()) -&gt; true | false
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(L::[binary()], S::binary()) -&gt; binary()
</code></pre>
<br />

join a list of binaries with the given separator

Example:

```erlang
<<"toto-tata-titi">> = bucbinary:join([<<"toto">>, <<"tata">>, <<"titi">>], <<"-">>).
```

<a name="trim-2"></a>

### trim/2 ###

<pre><code>
trim(Binary::binary(), X2::left | right | both) -&gt; binary()
</code></pre>
<br />

