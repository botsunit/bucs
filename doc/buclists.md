

# Module buclists #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_if-2">delete_if/2</a></td><td></td></tr><tr><td valign="top"><a href="#keyfind-3">keyfind/3</a></td><td>Equivalent to <a href="#keyfind-4"><tt>keyfind(Key, N, TupleList, false)</tt></a>.</td></tr><tr><td valign="top"><a href="#keyfind-4">keyfind/4</a></td><td></td></tr><tr><td valign="top"><a href="#keyfind-5">keyfind/5</a></td><td></td></tr><tr><td valign="top"><a href="#keyufind-3">keyufind/3</a></td><td>Equivalent to <a href="#keyufind-4"><tt>keyufind(Key, N, TupleList, false)</tt></a>.</td></tr><tr><td valign="top"><a href="#keyufind-4">keyufind/4</a></td><td></td></tr><tr><td valign="top"><a href="#merge_keylists-3">merge_keylists/3</a></td><td>
Merge the two keylists.</td></tr><tr><td valign="top"><a href="#nsplit-2">nsplit/2</a></td><td> 
Split the given list in N lists.</td></tr><tr><td valign="top"><a href="#pipemap-2">pipemap/2</a></td><td></td></tr><tr><td valign="top"><a href="#splitn-2">splitn/2</a></td><td> 
Split the given list in multiples lists of maximum N elements.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete_if-2"></a>

### delete_if/2 ###

`delete_if(Fun, List) -> any()`

<a name="keyfind-3"></a>

### keyfind/3 ###

<pre><code>
keyfind(Key::term(), N::integer(), TupleList::[tuple()]) -&gt; term()
</code></pre>
<br />

Equivalent to [`keyfind(Key, N, TupleList, false)`](#keyfind-4).

<a name="keyfind-4"></a>

### keyfind/4 ###

<pre><code>
keyfind(Key::term(), N::integer(), TupleList::[tuple()], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="keyfind-5"></a>

### keyfind/5 ###

`keyfind(Key, N, TupleList, M, Default) -> any()`

<a name="keyufind-3"></a>

### keyufind/3 ###

<pre><code>
keyufind(Key::term(), N::integer(), TupleList::[tuple()]) -&gt; term()
</code></pre>
<br />

Equivalent to [`keyufind(Key, N, TupleList, false)`](#keyufind-4).

<a name="keyufind-4"></a>

### keyufind/4 ###

<pre><code>
keyufind(Key::term(), N::integer(), TupleList::[tuple()], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="merge_keylists-3"></a>

### merge_keylists/3 ###

`merge_keylists(N, Rest, TupleList2) -> any()`

Merge the two keylists.

Example:

```

  Args = [{a, 1}, {b, 2}],
  Default = [{b, 3}, {c, 4}],
  elists:merge_keylists(1, Args, Default),
    #=> [{c, 4}, {a, 1}, {b, 2}]
```

<a name="nsplit-2"></a>

### nsplit/2 ###

`nsplit(List, N) -> any()`


Split the given list in N lists

Example:

```

 buclists:nsplit([a,b,c,d,e,f,g], 3).
   #=> [[a,b],[c,d],[e,f,g]]
```

<a name="pipemap-2"></a>

### pipemap/2 ###

<pre><code>
pipemap(Funs::list(), List::list()) -&gt; list()
</code></pre>
<br />

<a name="splitn-2"></a>

### splitn/2 ###

`splitn(List, N) -> any()`


Split the given list in multiples lists of maximum N elements.

Example:

```

 buclists:splitn([a,b,c,d,e,f,g], 3).
   #=> [[a,b,c],[d,e,f],[g]]
```

