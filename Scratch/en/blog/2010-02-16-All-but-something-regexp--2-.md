-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-02-16
title: Pragmatic Regular Expression Exclude (2)
author: Yann Esposito
authoruri: yannesposito.com
tags:  regexp, regular expression
-----

In my [previous post](previouspost) I had given some trick to match all except something. On the same idea, the trick to match the smallest possible string. Say you want to match the string between 'a' and 'b', for example, you want to match:

<pre class="twilight">
a.....<span class="Constant"><strong>a......b</strong></span>..b..a....<span class="Constant"><strong>a....b</strong></span>...
</pre>

Here are two common errors and a solution:

<pre class="twilight">
/a.*b/
<span class="Constant"><strong>a.....a......b..b..a....a....b</strong></span>...
</pre>

The first error is to use the *evil* `.*`. Because you will match from the first to the last.

<pre class="twilight">
/a.*?b/
<span class="Constant"><strong>a.....a......b</strong></span>..b..<span class="Constant"><strong>a....a....b</strong></span>...
</pre>

The next natural way, is to change the *greediness*. But it is not enough as you will match from the first `a` to the first `b`.
Then a simple constatation is that our matching string shouldn't contain any `a` nor `b`. Which lead to the last elegant solution.

<pre class="twilight">
/a[^ab]*b/
a.....<span class="Constant"><strong>a......b</strong></span>..b..a....<span class="Constant"><strong>a....b</strong></span>...
</pre>

Until now, that was, easy. 
Now, just pass at the case you need to match not between `a` and `b`, but between strings.
For example:
<div><code class="perl">
<li>...<li>
</code></div>

This is a bit difficult. You need to match 
<div><code class="perl">
<li>[anything not containing <li>]</li>
</code></div>

The first method would be to use the same reasoning as in my [previous post](previouspost). Here is a first try:

<div><code class="perl">
<li>([^<]|<[^l]|<l[^i]|<li[^>])*</li>
</code></div>

But what about the following string: 
<div><code class="perl">
<li>...<li</li>
</code></div>

That string should not match. This is why if we really want to match it correctly<sup><a href="#note1">&dagger;</a></sup> we need to add:
<div><code class="perl">
<li>([^<]|<[^l]|<l[^i]|<li[^>])*(|<|<l|<li)</li>
</code></div>

Yes a bit complicated. But what if the string I wanted to match was even longer?

Here is the algorithm way to handle this easily. You reduce the problem to the first one letter matching:

<div><code class="perl">
# transform a simple randomly choosen character
# to an unique ID 
# (you should verify the identifier is REALLY unique)
# beware the unique ID must not contain the 
# choosen character
s/X/_was_x_/g
s/Y/_was_y_/g

# transform the long string in this simple character
s/<li>/X/g
s/<\/li>/Y/g

# use the first method
s/X([^X]*)Y//g

# retransform choosen letter by string
s/X/<li>/g
s/Y/<\/li>/g

# retransform the choosen character back
s/_was_x_/X/g
s/_was_y_/Y/g
</code></div>

And it works in only 9 lines for any beginning and ending string. This solution should look less *I AM THE GREAT REGEXP M45T3R, URAN00B*, but is more convenient in my humble opinion. Further more, using this last solution prove you master regexp, because you know it is difficult to manage such problems with only a regexp.

---

<small><a name="note1"><sup>&dagger;</sup></a> I know I used an HTML syntax example, but in my real life usage, I needed to match between `en:` and `::`. And sometimes the string could finish with `e::`.</small>


[previouspost]: /Scratch/en/blog/2010-02-16-All-but-something-regexp--2- "All but something regexp"
