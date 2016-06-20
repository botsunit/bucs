

# Module bucdate #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Format dates in erlang.

Copyright (c) Dale Harvey

<a name="description"></a>

## Description ##

Licensed under the MIT license

This module formats erlang dates in the form {{Year, Month, Day},
{Hour, Minute, Second}} to printable strings, using (almost)
equivalent formatting rules as http://uk.php.net/date, US vs
European dates are disambiguated in the same way as
http://uk.php.net/manual/en/function.strtotime.php That is, Dates
in the m/d/y or d-m-y formats are disambiguated by looking at the
separator between the various components: if the separator is a
slash (/), then the American m/d/y is assumed; whereas if the
separator is a dash (-) or a dot (.), then the European d-m-y
format is assumed. To avoid potential ambiguity, it's best to use
ISO 8601 (YYYY-MM-DD) dates.

erlang has no concept of timezone so the following
formats are not implemented: B e I O P T Z
formats c and r will also differ slightly

See tests at bottom for examples
<a name="types"></a>

## Data Types ##




### <a name="type-dt_unit">dt_unit()</a> ###


<pre><code>
dt_unit() = second | minute | hour | day | month | year
</code></pre>




### <a name="type-dt_units">dt_units()</a> ###


<pre><code>
dt_units() = seconds | minutes | hours | days | months | years
</code></pre>




### <a name="type-now">now()</a> ###


<pre><code>
now() = {integer(), integer(), integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>
Add 1 unit to the given <tt>DateTime</tt>.</td></tr><tr><td valign="top"><a href="#add-3">add/3</a></td><td>
Add <tt>N</tt> units to the given <tt>DateTime</tt>.</td></tr><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td>
Compate <tt>Date1</tt> and <tt>Date2</tt></td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>format current local time as Format.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td>format Date as Format.</td></tr><tr><td valign="top"><a href="#local_timezone-0">local_timezone/0</a></td><td>
Return the local timezone.</td></tr><tr><td valign="top"><a href="#nparse-1">nparse/1</a></td><td>parses the datetime from a string into 'now' format.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>parses the datetime from a string.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>parses the datetime from a string.</td></tr><tr><td valign="top"><a href="#timezone_offset-0">timezone_offset/0</a></td><td>
Returns the time difference between UTC time and local time, in minutes.</td></tr><tr><td valign="top"><a href="#to_iso8601-1">to_iso8601/1</a></td><td>
Return date using iso8601 format.</td></tr><tr><td valign="top"><a href="#today-0">today/0</a></td><td>
return now local datetime.</td></tr><tr><td valign="top"><a href="#today_utc-0">today_utc/0</a></td><td>
return now UTC datetime.</td></tr><tr><td valign="top"><a href="#tomorrow-0">tomorrow/0</a></td><td>
return tomorrow local datetime.</td></tr><tr><td valign="top"><a href="#tomorrow_utc-0">tomorrow_utc/0</a></td><td>
return tomorrow UTC datetime.</td></tr><tr><td valign="top"><a href="#yesterday-0">yesterday/0</a></td><td>
return yesterday local datetime.</td></tr><tr><td valign="top"><a href="#yesterday_utc-0">yesterday_utc/0</a></td><td>
return yesterday UTC datetime.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Date::<a href="calendar.md#type-datetime">calendar:datetime()</a>, N::<a href="#type-dt_unit">dt_unit()</a>) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

Add 1 unit to the given `DateTime`.

<a name="add-3"></a>

### add/3 ###

<pre><code>
add(DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>, N::integer(), X3::<a href="#type-dt_units">dt_units()</a>) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

Add `N` units to the given `DateTime`.

<a name="compare-2"></a>

### compare/2 ###

<pre><code>
compare(Date1::<a href="calendar.md#type-datetime">calendar:datetime()</a>, Date2::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; -1 | 1 | 0
</code></pre>
<br />

Compate `Date1` and `Date2`

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Format::string()) -&gt; string()
</code></pre>
<br />

format current local time as Format

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(Format::string(), Now::<a href="calendar.md#type-datetime">calendar:datetime()</a> | <a href="#type-now">now()</a>) -&gt; string()
</code></pre>
<br />

format Date as Format

<a name="local_timezone-0"></a>

### local_timezone/0 ###

`local_timezone() -> any()`

Return the local timezone

<a name="nparse-1"></a>

### nparse/1 ###

<pre><code>
nparse(Date::string()) -&gt; <a href="#type-now">now()</a>
</code></pre>
<br />

parses the datetime from a string into 'now' format

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Date::string()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

parses the datetime from a string

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Date::string(), Now::<a href="calendar.md#type-datetime">calendar:datetime()</a> | <a href="#type-now">now()</a>) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

parses the datetime from a string

<a name="timezone_offset-0"></a>

### timezone_offset/0 ###

`timezone_offset() -> any()`

Returns the time difference between UTC time and local time, in minutes.

<a name="to_iso8601-1"></a>

### to_iso8601/1 ###

<pre><code>
to_iso8601(Date::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; string()
</code></pre>
<br />

Return date using iso8601 format

<a name="today-0"></a>

### today/0 ###

<pre><code>
today() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return now local datetime.

<a name="today_utc-0"></a>

### today_utc/0 ###

<pre><code>
today_utc() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return now UTC datetime.

<a name="tomorrow-0"></a>

### tomorrow/0 ###

<pre><code>
tomorrow() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return tomorrow local datetime.

<a name="tomorrow_utc-0"></a>

### tomorrow_utc/0 ###

<pre><code>
tomorrow_utc() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return tomorrow UTC datetime.

<a name="yesterday-0"></a>

### yesterday/0 ###

<pre><code>
yesterday() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return yesterday local datetime.

<a name="yesterday_utc-0"></a>

### yesterday_utc/0 ###

<pre><code>
yesterday_utc() -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

return yesterday UTC datetime.

