

# Module bucstring #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#gsub-3">gsub/3</a></td><td>
Return an new string with the all occurances of Old substitued by New.</td></tr><tr><td valign="top"><a href="#sub-3">sub/3</a></td><td>
Return an new string with the first occurance of Old substitued by New.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="gsub-3"></a>

### gsub/3 ###

<pre><code>
gsub(Str::string(), Old::string(), New::string()) -&gt; string()
</code></pre>
<br />

Return an new string with the all occurances of Old substitued by New

Example:

```erlang

  "HeLLo WorLd" = estring:gsub("Hello World", "l", "L").
```

<a name="sub-3"></a>

### sub/3 ###

<pre><code>
sub(Str::string(), Old::string(), New::string()) -&gt; string()
</code></pre>
<br />

Return an new string with the first occurance of Old substitued by New

Example:

```erlang

  "HeLlo World" = estring:sub("Hello World", "l", "L").
```

