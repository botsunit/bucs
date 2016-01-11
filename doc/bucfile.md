

# Module bucfile #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#copy-2">copy/2</a></td><td>Equivalent to <a href="#copy-3"><tt>copy(Source, Destination, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#copy-3">copy/3</a></td><td>
Copy a <tt>Source</tt> to a <tt>Destination</tt></td></tr><tr><td valign="top"><a href="#expand_path-1">expand_path/1</a></td><td>
Expand the given path.</td></tr><tr><td valign="top"><a href="#make_dir-1">make_dir/1</a></td><td>
Create the given directory if it not exist.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_path-1">normalize_path/1</a></td><td>
Normalize the given path.</td></tr><tr><td valign="top"><a href="#realpath-1">realpath/1</a></td><td>
Return the realpath of the given path.</td></tr><tr><td valign="top"><a href="#relative_from-2">relative_from/2</a></td><td>
Return the given <tt>FilePath</tt> relatively to the <tt>FromPath</tt>.</td></tr><tr><td valign="top"><a href="#remove_recursive-1">remove_recursive/1</a></td><td>
Remove, recursively the given path.</td></tr><tr><td valign="top"><a href="#user_home-0">user_home/0</a></td><td>
Return the HOME directory.</td></tr><tr><td valign="top"><a href="#wildcard-2">wildcard/2</a></td><td>
Same as <tt>filelib:wildcard/1</tt> but where files listed in <tt>Exclude</tt> are excluded.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="copy-2"></a>

### copy/2 ###

`copy(Source, Destination) -> any()`

Equivalent to [`copy(Source, Destination, [])`](#copy-3).

<a name="copy-3"></a>

### copy/3 ###

`copy(Source, Destination, Options) -> any()`

Copy a `Source` to a `Destination`

Available options:

* `recursive`

* `{exclude, [file:filename()]}`

* `{only, [file:filename()]}`


<a name="expand_path-1"></a>

### expand_path/1 ###

<pre><code>
expand_path(Path::string() | binary()) -&gt; binary() | list()
</code></pre>
<br />

Expand the given path

Example:

```erlang

  "/home/user" = bucfile:expand_path("~").<<"/home/user">> = bucfile:expand_path(<<"~">>).
```

<a name="make_dir-1"></a>

### make_dir/1 ###

`make_dir(Path) -> any()`

Create the given directory if it not exist

<a name="match-2"></a>

### match/2 ###

`match(Path, Exp) -> any()`

<a name="normalize_path-1"></a>

### normalize_path/1 ###

<pre><code>
normalize_path(Path::string() | binary()) -&gt; string() | binary()
</code></pre>
<br />

Normalize the given path

Example:

```erlang

  "/" = bucfile:normalize_path("/toto/tutu/../../../../../..").<<"/">> = bucfile:normalize_path(<<"/toto/tutu/../../../../../..">>).
  "/toto/titi" = bucfile:normalize_path("/toto/tata/../titi").
```

<a name="realpath-1"></a>

### realpath/1 ###

`realpath(Path) -> any()`

Return the realpath of the given path

<a name="relative_from-2"></a>

### relative_from/2 ###

`relative_from(FilePath, FromPath) -> any()`

Return the given `FilePath` relatively to the `FromPath`.

<a name="remove_recursive-1"></a>

### remove_recursive/1 ###

`remove_recursive(Path) -> any()`

Remove, recursively the given path

<a name="user_home-0"></a>

### user_home/0 ###

<pre><code>
user_home() -&gt; string()
</code></pre>
<br />

Return the HOME directory

Example:

```erlang

  "/home/user" = bucfile:user_home().
```

<a name="wildcard-2"></a>

### wildcard/2 ###

`wildcard(Path, Exclude) -> any()`

Same as `filelib:wildcard/1` but where files listed in `Exclude` are excluded.

