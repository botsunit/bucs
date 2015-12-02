

# Module buctimer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-datetime">datetime()</a> ###


<pre><code>
datetime() = {<a href="calendar.md#type-year">calendar:year()</a>, <a href="calendar.md#type-month">calendar:month()</a>, <a href="calendar.md#type-day">calendar:day()</a>, <a href="calendar.md#type-hour">calendar:hour()</a>, <a href="calendar.md#type-minute">calendar:minute()</a>, <a href="calendar.md#type-second">calendar:second()</a>}
</code></pre>




### <a name="type-day">day()</a> ###


<pre><code>
day() = <a href="calendar.md#type-day">calendar:day()</a> | [<a href="calendar.md#type-day">calendar:day()</a>] | <a href="#type-period">period()</a> | [<a href="#type-day_of_week">day_of_week()</a>]
</code></pre>




### <a name="type-day_of_week">day_of_week()</a> ###


<pre><code>
day_of_week() = monday | thursday | wednesday | thursday | friday | saturday | sunday
</code></pre>




### <a name="type-hour">hour()</a> ###


<pre><code>
hour() = <a href="calendar.md#type-hour">calendar:hour()</a> | [<a href="calendar.md#type-hour">calendar:hour()</a>] | <a href="#type-period">period()</a>
</code></pre>




### <a name="type-minute">minute()</a> ###


<pre><code>
minute() = <a href="calendar.md#type-minute">calendar:minute()</a> | [<a href="calendar.md#type-minute">calendar:minute()</a>] | <a href="#type-period">period()</a>
</code></pre>




### <a name="type-month">month()</a> ###


<pre><code>
month() = <a href="calendar.md#type-month">calendar:month()</a> | [<a href="calendar.md#type-month">calendar:month()</a>] | <a href="#type-period">period()</a>
</code></pre>




### <a name="type-period">period()</a> ###


<pre><code>
period() = atom()
</code></pre>




### <a name="type-second">second()</a> ###


<pre><code>
second() = <a href="calendar.md#type-second">calendar:second()</a> | [<a href="calendar.md#type-second">calendar:second()</a>] | <a href="#type-period">period()</a>
</code></pre>




### <a name="type-timer_spec">timer_spec()</a> ###


<pre><code>
timer_spec() = {<a href="#type-year">year()</a>, <a href="#type-month">month()</a>, <a href="#type-day">day()</a>, <a href="#type-hour">hour()</a>, <a href="#type-minute">minute()</a>, <a href="#type-second">second()</a>}
</code></pre>




### <a name="type-year">year()</a> ###


<pre><code>
year() = <a href="calendar.md#type-year">calendar:year()</a> | [<a href="calendar.md#type-year">calendar:year()</a>] | <a href="#type-period">period()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#next-1">next/1</a></td><td>
Return the next datetime from now.</td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td>
Return the next datetime from the given datetime.</td></tr><tr><td valign="top"><a href="#verify-1">verify/1</a></td><td>
Verify the timer syntax.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="next-1"></a>

### next/1 ###

<pre><code>
next(Spec::<a href="#type-timer_spec">timer_spec()</a>) -&gt; {ok, <a href="calendar.md#type-datetime">calendar:datetime()</a>, <a href="calendar.md#type-second">calendar:second()</a>} | {error, term()} | stop
</code></pre>
<br />

Return the next datetime from now.

<a name="next-2"></a>

### next/2 ###

<pre><code>
next(Spec::<a href="#type-timer_spec">timer_spec()</a>, From::<a href="calendar.md#type-datetime">calendar:datetime()</a> | <a href="#type-datetime">datetime()</a>) -&gt; {ok, <a href="calendar.md#type-datetime">calendar:datetime()</a>, <a href="calendar.md#type-second">calendar:second()</a>} | {error, term()} | stop
</code></pre>
<br />

Return the next datetime from the given datetime.

<a name="verify-1"></a>

### verify/1 ###

<pre><code>
verify(Spec::<a href="#type-timer_spec">timer_spec()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Verify the timer syntax.

