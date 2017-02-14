

# Module bucbinary #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
join a list of binaries with the given separator.</td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

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

