

# Module bucrandom #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2016 BotsUnit

Erlang server for generating random strings, usable as file names, tokens,...

The implementation avoids problems with "rand-seeding"
the Erlang random generator between processes : Using bucrandom:randstr/1
guarantees a good random distribution, and low probability of getting
the same value at first call, from wherever the function is called.
However, it may not be appropriate for cryptographic or 'sensible' purposes.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Grégoire Lejeune ([`gregoire.lejeune@botsunit.com`](mailto:gregoire.lejeune@botsunit.com)), Mathias Franck ([`mathias.franck@botsunit.com`](mailto:mathias.franck@botsunit.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#randstr-1">randstr/1</a></td><td> 
Returns a random string of a given length, 
that contains only letters (no diacritics) or digits.</td></tr><tr><td valign="top"><a href="#randstr-2">randstr/2</a></td><td>
Returns a random string of a given length,
that contains only <tt>Allowed</tt> chars.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="randstr-1"></a>

### randstr/1 ###

<pre><code>
randstr(Length::integer()) -&gt; string()
</code></pre>
<br />


Returns a random string of a given length, 
that contains only letters (no diacritics) or digits

Param:

* Length: the length of the string to generate

Example:

```erlang

 1> bucrandom:randstr(12).
 "ZL7YmS5HRQod"
```

<a name="randstr-2"></a>

### randstr/2 ###

<pre><code>
randstr(Length::integer(), Allowed::list()) -&gt; string()
</code></pre>
<br />

Returns a random string of a given length,
that contains only `Allowed` chars.

Param:

* Length: the length of the string to generate 
* Allowed: List of chars.

Example:

```erlang

 1> bucrandom:randstr(12, "01").
 "010011010011"
```

