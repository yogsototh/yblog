---
kind:           article
published:      2013-10-09
image: https://yogsototh.github.io/parsec-presentation/parsec/img/mp/AST.png
en: title: Parsec Presentation
fr: title: Parsec Presentation
author: Yann Esposito
authoruri: yannesposito.com
layout: article2
tags: programming, haskell, parsec, parser
theme: scientific
---

![AST](https://yogsototh.github.io/parsec-presentation/parsec/img/mp/AST.png)\

<div class="intro">

en: %tldr Short introduction to Parsec for beginner.

fr: %tlal Une introduction rapide à Parsec. Un parser en Haskell.

</div>

- The %html presentation is [here](https://yogsototh.github.io/parsec-presentation/parsec.html).

<div style="display:none">
\(\newcommand{\F}{\mathbf{F}}\)
\(\newcommand{\E}{\mathbf{E}}\)
\(\newcommand{\C}{\mathcal{C}}\)
\(\newcommand{\D}{\mathcal{D}}\)
\(\newcommand{\id}{\mathrm{id}}\)
\(\newcommand{\ob}[1]{\mathrm{ob}(#1)}\)
\(\newcommand{\hom}[1]{\mathrm{hom}(#1)}\)
\(\newcommand{\Set}{\mathbf{Set}}\)
\(\newcommand{\Mon}{\mathbf{Mon}}\)
\(\newcommand{\Vec}{\mathbf{Vec}}\)
\(\newcommand{\Grp}{\mathbf{Grp}}\)
\(\newcommand{\Rng}{\mathbf{Rng}}\)
\(\newcommand{\ML}{\mathbf{ML}}\)
\(\newcommand{\Hask}{\mathbf{Hask}}\)
\(\newcommand{\Cat}{\mathbf{Cat}}\)
\(\newcommand{\fmap}{\mathtt{fmap}}\)
</div>

<!-- Begin slides. Just make elements with a class of slide. -->

<section class="slide">
<div style="text-align:center; font-size: .9em; width: 100%; line-height: 1.2em">
<h1 style="position: relative;">Parsec</h1>
<author><em class="base1">by</em> Yann Esposito</author>
<div style="font-size:.5em; margin: 0 1em;">
<twitter>
<a href="http://twitter.com/yogsototh">@yogsototh</a>,
</twitter>
<googleplus>
<a href="https://plus.google.com/117858550730178181663">+yogsototh</a>
</googleplus>
</div>
<div style="font-size:.8em">
<em class="base1">for</em>
<a href="http://www.meetup.com/riviera-scala-clojure">
Riviera Scala Clojure Meetup (Haskell)
</a><br/>
<em class="base1">the</em> <ti style="font-size: .8em">8 Oct 2013</ti>
</div>
</div>
</section>
<section class="slide">
<h2 id="parsing">Parsing</h2>
<p>Latin pars (ōrātiōnis), meaning part (of speech).</p>
<ul>
<li><strong>analysing a string of symbols</strong></li>
<li><strong>formal grammar</strong>.</li>
</ul>
</section>
<section class="slide">
<h2 id="parsing-in-programming-languages">Parsing in Programming Languages</h2>
<p>Complexity:</p>
<table>
<thead>
<tr class="header">
<th align="left">Method</th>
<th align="left">Typical Example</th>
<th align="left">Output Data Structure</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Splitting</td>
<td align="left">CSV</td>
<td align="left">Array, Map</td>
</tr>
<tr class="even">
<td align="left">Regexp</td>
<td align="left">email</td>
<td align="left">+ Fixed Layout Tree</td>
</tr>
<tr class="odd">
<td align="left">Parser</td>
<td align="left">Programming language</td>
<td align="left">+ Most Data Structure</td>
</tr>
</tbody>
</table>
</section>
<section class="slide">
<h2 id="parser-culture">Parser <span class="and">&amp;</span> culture</h2>
<p>In Haskell Parser are really easy to use.</p>
<p>Generally:</p>
<ul>
<li>In most languages: <strong>split</strong> then <strong>regexp</strong> then <strong>parse</strong></li>
<li>In Haskell: <strong>split</strong> then <strong>parse</strong></li>
</ul>
</section>
<section class="slide">
<h2 id="parsing-example">Parsing Example</h2>
<p>From String:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">(<span class="dv">1</span><span class="fu">+</span><span class="dv">3</span>)<span class="fu">*</span>(<span class="dv">1</span><span class="fu">+</span><span class="dv">5</span><span class="fu">+</span><span class="dv">9</span>)</code></pre>
<p>To data structure:</p>
<p><img src="https://yogsototh.github.io/parsec-presentation/parsec/img/mp/AST.png" alt="AST" /><br /></p>
</section>
<section class="slide">
<h2 id="parsec">Parsec</h2>
<blockquote>
<p>Parsec lets you construct parsers by combining high-order Combinators to create larger expressions.</p>
<p>Combinator parsers are written and used within the same programming language as the rest of the program.</p>
<p>The parsers are first-class citizens of the languages [...]&quot;</p>
<p><em><a href="http://www.haskell.org/haskellwiki/Parsec">Haskell Wiki</a></em></p>
</blockquote>
</section>
<section class="slide">
<h2 id="parser-libraries">Parser Libraries</h2>
<p>In reality there are many choices:</p>
<table>
<tbody>
<tr class="odd">
<td align="left">attoparsec</td>
<td align="left">fast</td>
</tr>
<tr class="even">
<td align="left">Bytestring-lexing</td>
<td align="left">fast</td>
</tr>
<tr class="odd">
<td align="left">Parsec 3</td>
<td align="left">powerful, nice error reporting</td>
</tr>
</tbody>
</table>
</section>
<section class="slide">
<h2 id="haskell-remarks-1">Haskell Remarks (1)</h2>
<p>spaces are meaningful</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">f x   <span class="co">-- ⇔ f(x) in C-like languages</span>
f x y <span class="co">-- ⇔ f(x,y)</span></code></pre>
</section>
<section class="slide">
<h2 id="haskell-remarks-2">Haskell Remarks (2)</h2>
<p>Don't mind strange operators (<code>&lt;*&gt;</code>, <code>&lt;\$&gt;</code>).<br />Consider them like separators, typically commas.<br />They are just here to deal with types.</p>
<p>Informally:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">toto <span class="fu">&lt;$&gt;</span> x <span class="fu">&lt;*&gt;</span> y <span class="fu">&lt;*&gt;</span> z
    <span class="co">-- ⇔ toto x y z</span>
    <span class="co">-- ⇔ toto(x,y,z) in C-like languages</span></code></pre>
</section>
<section class="slide">
<h2 id="minimal-parsec-examples">Minimal Parsec Examples</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell">whitespaces <span class="fu">=</span> many (oneOf <span class="st">&quot;\t &quot;</span>)
number <span class="fu">=</span> many1 digit
symbol <span class="fu">=</span> oneOf <span class="st">&quot;!#$%<span class="and">&amp;</span>|*+-/:&lt;=&gt;?@^_~&quot;</span></code></pre>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="st">&quot;   \t &quot;</span>  <span class="co">-- whitespaces on &quot;   \t &quot;</span>
<span class="st">&quot;&quot;</span>        <span class="co">-- whitespaces on &quot;32&quot;</span>
<span class="st">&quot;32&quot;</span>      <span class="co">-- number on &quot;32&quot;</span>

<span class="co">-- number on &quot;   \t 32  &quot;</span>
<span class="st">&quot;number&quot;</span> (line <span class="dv">1</span>, column <span class="dv">1</span>)<span class="fu">:</span>
unexpected <span class="st">&quot; &quot;</span>
expecting digit</code></pre>
</section>
<section class="slide">
<h2 id="comparison-with-regexp-parsec">Comparison with Regexp (Parsec)</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">IP</span> <span class="fu">=</span> <span class="dt">IP</span> <span class="dt">Word8</span> <span class="dt">Word8</span> <span class="dt">Word8</span> <span class="dt">Word8</span>
ip <span class="fu">=</span> <span class="dt">IP</span> <span class="fu">&lt;$&gt;</span>                             <span class="co">-- number.number.number.number</span>
       number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>          <span class="co">-- put that in an IP data type</span>
       number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>
       number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>
       number
number <span class="fu">=</span> <span class="kw">do</span>
    x <span class="ot">&lt;-</span> <span class="fu">read</span> <span class="fu">&lt;$&gt;</span> many1 digit <span class="co">-- read the number</span>
    guard (<span class="dv">0</span> <span class="fu">&lt;=</span> x <span class="fu"><span class="and">&amp;</span><span class="and">&amp;</span></span> x <span class="fu">&lt;</span> <span class="dv">256</span>) <span class="co">-- ensure it is 0 &lt;= x &lt; 256</span>
    <span class="fu">return</span> (<span class="fu">fromIntegral</span> x)   <span class="co">-- returns a Word8</span></code></pre>
</section>
<section class="slide">
<h2 id="comparison-with-regexp-perl-regexp">Comparison with Regexp (Perl Regexp)</h2>
<pre class="sourceCode perl"><code class="sourceCode perl"><span class="co"># remark: 888.999.999.999 is accepted</span>
\b\d{<span class="dv">1</span>,<span class="dv">3</span>}\.\d{<span class="dv">1</span>,<span class="dv">3</span>}\.\d{<span class="dv">1</span>,<span class="dv">3</span>}\.\d{<span class="dv">1</span>,<span class="dv">3</span>}\b

<span class="co"># exact but difficult to read</span>
\b(?:(?:<span class="dv">25</span>[<span class="dv">0-5</span>]|<span class="dv">2</span>[<span class="dv">0-4</span>][<span class="dv">0-9</span>]|[<span class="dv">01</span>]?[<span class="dv">0-9</span>][<span class="dv">0-9</span>]?)\.){<span class="dv">3</span>}
  (?:<span class="dv">25</span>[<span class="dv">0-5</span>]|<span class="dv">2</span>[<span class="dv">0-4</span>][<span class="dv">0-9</span>]|[<span class="dv">01</span>]?[<span class="dv">0-9</span>][<span class="dv">0-9</span>]?)\b</code></pre>
<p>Also, regexp are <em>unityped</em> by nature.</p>
</section>
<section class="slide">
<h2 id="monadic-style">Monadic style</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">number ::</span> <span class="dt">Parser</span> <span class="dt">String</span>
number <span class="fu">=</span> many1 digit

<span class="ot">number&#39; ::</span> <span class="dt">Parser</span> <span class="dt">Int</span>
number&#39; <span class="fu">=</span> <span class="kw">do</span>
    <span class="co">-- digitString :: String</span>
    digitString <span class="ot">&lt;-</span> many1 digit
    <span class="fu">return</span> (<span class="fu">read</span> digitString)</code></pre>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="st">&quot;32&quot;</span><span class="ot"> ::</span> [<span class="dt">Char</span>]  <span class="co">-- number on &quot;32&quot;</span>
<span class="dv">32</span><span class="ot">   ::</span> <span class="dt">Int</span>     <span class="co">-- number&#39; on &quot;32&quot;</span></code></pre>
</section>
<section class="slide">
<h2 id="combining-monadic-style-s-asb-ε">Combining Monadic style (S = aSb | ε)</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell">s <span class="fu">=</span> <span class="kw">do</span>
        a <span class="ot">&lt;-</span> string <span class="st">&quot;a&quot;</span>
        mid <span class="ot">&lt;-</span> s
        b <span class="ot">&lt;-</span> string <span class="st">&quot;b&quot;</span>
        <span class="fu">return</span> (a <span class="fu">++</span> mid <span class="fu">++</span> b)
    <span class="fu">&lt;|&gt;</span> string <span class="st">&quot;&quot;</span></code></pre>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="st">&quot;&quot;</span>          <span class="co">-- s on &quot;&quot;</span>
<span class="st">&quot;aaabbb&quot;</span>    <span class="co">-- s on &quot;aaabbb&quot;</span>
<span class="st">&quot;aabb&quot;</span>      <span class="co">-- s on &quot;aabbb&quot;</span>
<span class="co">-- s on &quot;aaabb&quot;</span>
<span class="dt">S</span> (line1 <span class="dv">1</span>, column <span class="dv">4</span>)<span class="fu">:</span>
unexpected end <span class="kw">of</span> input
expecting <span class="st">&quot;b&quot;</span></code></pre>
</section>
<section class="slide">
<h2 id="combining-applicative-style-s-asb-ε">Combining Applicative style (S = aSb | ε)</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell">s <span class="fu">=</span> concat3 <span class="fu">&lt;$&gt;</span> string <span class="st">&quot;a&quot;</span> <span class="fu">&lt;*&gt;</span> s <span class="fu">&lt;*&gt;</span> char <span class="st">&quot;b&quot;</span>
    <span class="fu">&lt;|&gt;</span> string <span class="st">&quot;&quot;</span>
    <span class="kw">where</span>
        concat3 x y z <span class="fu">=</span> x <span class="fu">++</span> y <span class="fu">++</span> z</code></pre>
</section>
<section class="slide">
<h2 id="applicative-style-usefull-with-data-types">Applicative Style usefull with Data types</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">IP</span> <span class="fu">=</span> <span class="dt">IP</span> <span class="dt">Word8</span> <span class="dt">Word8</span> <span class="dt">Word8</span> <span class="dt">Word8</span>

parseIP <span class="fu">=</span> <span class="dt">IP</span> <span class="fu">&lt;$&gt;</span>
            number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>
            number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>
            number <span class="fu">&lt;*</span>  char <span class="ch">&#39;.&#39;</span> <span class="fu">&lt;*&gt;</span>
            number

monadicParseIP <span class="fu">=</span> <span class="kw">do</span>
    d1 <span class="ot">&lt;-</span> number
    char <span class="ch">&#39;.&#39;</span>
    d2 <span class="ot">&lt;-</span> number
    char <span class="ch">&#39;.&#39;</span>
    d3 <span class="ot">&lt;-</span> number
    char <span class="ch">&#39;.&#39;</span>
    d4 <span class="ot">&lt;-</span> number
    <span class="fu">return</span> (<span class="dt">IP</span> d1 d2 d3 d4)</code></pre>
</section>
<section class="slide">
<h2 id="write-number-correctly">Write number correctly</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">number ::</span> <span class="dt">Parser</span> <span class="dt">Word8</span>
number <span class="fu">=</span> <span class="kw">do</span>
    x <span class="ot">&lt;-</span> <span class="fu">read</span> <span class="fu">&lt;$&gt;</span> many1 digit
    guard (<span class="dv">0</span> <span class="fu">&lt;=</span> x <span class="fu"><span class="and">&amp;</span><span class="and">&amp;</span></span> x <span class="fu">&lt;</span> <span class="dv">256</span>) <span class="fu">&lt;?&gt;</span>
        <span class="st">&quot;Number between 0 and 255 (here &quot;</span> <span class="fu">++</span> <span class="fu">show</span> x <span class="fu">++</span> <span class="st">&quot;)&quot;</span>
    <span class="fu">return</span> (<span class="fu">fromIntegral</span> x)</code></pre>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="fu">&gt;&gt;&gt;</span> test parseIP <span class="st">&quot;parseIP&quot;</span> <span class="st">&quot;823.32.80.113&quot;</span>
<span class="st">&quot;parseIP&quot;</span> (line <span class="dv">1</span>, column <span class="dv">4</span>)<span class="fu">:</span>
unexpected <span class="st">&quot;.&quot;</span>
expecting digit <span class="fu">or</span> <span class="dt">Number</span> between <span class="dv">0</span> <span class="fu">and</span> <span class="dv">255</span> (here <span class="dv">823</span>)</code></pre>
</section>
<section class="slide">
<h2 id="so">So</h2>
<ul>
<li>combination of simple parsers</li>
<li>error messages with <code>(&lt;?&gt;)</code></li>
<li>embed result in data type using Applicative style</li>
<li>Not shown, use another monad with the parser</li>
</ul>
<p>Time to do something cool</p>
</section>
<section class="slide">
<h2 id="useful-definition">Useful definition</h2>
<p><code>try</code> tries to parse and backtracks if it fails.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">(<span class="fu">&lt;||&gt;</span>) parser1 parser2 <span class="fu">=</span> try parser1 <span class="fu">&lt;|&gt;</span> parser2</code></pre>
</section>
<section class="slide">
<h2 id="scheme">Scheme</h2>
<p><a href="https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours">Write Yourself a Scheme in 48 hours</a></p>
<p>Remember from text to data structure. Our data structure:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">LispVal</span> <span class="fu">=</span>    <span class="dt">Atom</span> <span class="dt">String</span>     <span class="co">-- print or val-3 ...</span>
                <span class="fu">|</span> <span class="dt">Number</span> <span class="dt">Integer</span>  <span class="co">-- 32</span>
                <span class="fu">|</span> <span class="dt">String</span> <span class="dt">String</span>   <span class="co">-- &quot;foo&quot;</span>
                <span class="fu">|</span> <span class="dt">Bool</span> <span class="dt">Bool</span>       <span class="co">-- #t or #f</span>
                <span class="fu">|</span> <span class="dt">List</span> [<span class="dt">LispVal</span>]  <span class="co">-- (print &quot;foo&quot; &quot;bar&quot;)</span></code></pre>
<p>Next will parse String, Atom, Integer</p>
</section>
<section class="slide">
<h2 id="parse-string">Parse String</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">parseString ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseString <span class="fu">=</span> <span class="kw">do</span>
    char <span class="ch">&#39;&quot;&#39;</span>
    x <span class="ot">&lt;-</span> many (noneOf <span class="st">&quot;\&quot;&quot;</span>)
    char <span class="ch">&#39;&quot;&#39;</span>
    <span class="fu">return</span> (<span class="dt">String</span> x)</code></pre>
<pre><code>-- parseString on &#39;&quot;toto&quot;&#39;
String &quot;toto&quot; :: LispVal
-- parseString on &#39;&quot; hello&quot;&#39;
String &quot; hello&quot; :: LispVal</code></pre>
</section>
<section class="slide">
<h2 id="parse-atom">Parse Atom</h2>
<p>In Scheme true is <code>#t</code> and false <code>#f</code>. Which are also valid Atoms.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">symbol ::</span> <span class="dt">Parser</span> <span class="dt">Char</span>
symbol <span class="fu">=</span> oneOf <span class="st">&quot;!#$%<span class="and">&amp;</span>|*+-/:&lt;=&gt;?@^_~&quot;</span>

<span class="ot">parseAtom ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseAtom <span class="fu">=</span> <span class="kw">do</span>
    first <span class="ot">&lt;-</span> letter <span class="fu">&lt;|&gt;</span> symbol
    rest <span class="ot">&lt;-</span> many (letter <span class="fu">&lt;|&gt;</span> digit <span class="fu">&lt;|&gt;</span> symbol)
    <span class="kw">let</span> atom <span class="fu">=</span> first<span class="fu">:</span>rest
    <span class="fu">return</span> <span class="fu">$</span> <span class="kw">case</span> atom <span class="kw">of</span>
                <span class="st">&quot;#t&quot;</span> <span class="ot">-&gt;</span> <span class="dt">Bool</span> <span class="kw">True</span>
                <span class="st">&quot;#f&quot;</span> <span class="ot">-&gt;</span> <span class="dt">Bool</span> <span class="kw">False</span>
                _    <span class="ot">-&gt;</span> <span class="dt">Atom</span> atom</code></pre>
</section>
<section class="slide">
<h2 id="test-parseatom">Test <code>parseAtom</code></h2>
<pre><code>-- parseAtom on &#39;#t&#39;
Bool True :: LispVal
-- parseAtom on &#39;#f&#39;
Bool False :: LispVal
-- parseAtom on &#39;some-atom&#39;
Atom &quot;some-atom&quot; :: LispVal</code></pre>
</section>
<section class="slide">
<h2 id="parse-number">Parse Number</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">parseNumber ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseNumber <span class="fu">=</span> <span class="dt">Number</span> <span class="fu">.</span> <span class="fu">read</span> <span class="fu">&lt;$&gt;</span> many1 digit</code></pre>
<pre><code>-- parseNumber on &#39;18&#39;
Number 18 :: LispVal
-- parseNumber on &#39;188930992344321234&#39;
Number 188930992344321234 :: LispVal</code></pre>
</section>
<section class="slide">
<h2 id="compose-all-parsers">Compose all parsers</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">parseExpr ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseExpr <span class="fu">=</span> parseAtom
            <span class="fu">&lt;||&gt;</span> parseString
            <span class="fu">&lt;||&gt;</span> parseNumber</code></pre>
</section>
<section class="slide">
<h2 id="test-the-parser">Test the parser</h2>
<pre><code>-- parseExpr on &#39;188930992344321234&#39;
Number 188930992344321234 :: LispVal
-- parseExpr on &#39;#t&#39;
Bool True :: LispVal
-- parseExpr on &#39;just-some-word&#39;
Atom &quot;just-some-word&quot; :: LispVal
-- parseExpr on &#39;%-symbol-start&#39;
Atom &quot;%-symbol-start&quot; :: LispVal
-- parseExpr on &#39;&quot;a String&quot;&#39;
String &quot;a String&quot; :: LispVal</code></pre>
</section>
<section class="slide">
<h2 id="recursive-parsers">Recursive Parsers</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">parseList ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseList <span class="fu">=</span> <span class="dt">List</span> <span class="fu">&lt;$&gt;</span>
    (char <span class="ch">&#39;(&#39;</span> <span class="fu">*&gt;</span> sepBy parseExpr&#39; spaces <span class="fu">&lt;*</span> char <span class="ch">&#39;)&#39;</span> )

<span class="ot">parseExpr&#39; ::</span> <span class="dt">Parser</span> <span class="dt">LispVal</span>
parseExpr&#39; <span class="fu">=</span> parseAtom
             <span class="fu">&lt;||&gt;</span> parseString
             <span class="fu">&lt;||&gt;</span> parseNumber
             <span class="fu">&lt;||&gt;</span> parseList</code></pre>
</section>
<section class="slide">
<h2 id="test-parse-list">Test Parse List</h2>
<pre><code>-- parseExpr&#39; on &#39;(foo (bar baz))&#39;
List [Atom &quot;foo&quot;,List [Atom &quot;bar&quot;,Atom &quot;baz&quot;]] :: LispVal

-- parseExpr&#39; on &#39;(foo (bar)&#39;
&quot;parseExpr&#39;&quot; (line 1, column 11):
unexpected end of input
expecting white space, letter, &quot;\&quot;&quot;, digit, &quot;(&quot; or &quot;)&quot;

-- parseExpr&#39; on &#39;(((foo)) bar)&#39;
List [List [List [Atom &quot;foo&quot;]],Atom &quot;bar&quot;] :: LispVal</code></pre>
</section>
<section class="slide">
<h2 id="remark">Remark</h2>
<p>Why Haskell?</p>
<p>Close relation between parser and data types.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">Product</span> a b <span class="fu">=</span> <span class="dt">Product</span> a b
<span class="kw">data</span> <span class="dt">Sum</span> a b <span class="fu">=</span> <span class="dt">A</span> a <span class="fu">|</span> <span class="dt">B</span> b</code></pre>
<pre class="sourceCode haskell"><code class="sourceCode haskell">productParser <span class="fu">=</span> <span class="kw">do</span>
                  parser1
                  parser2
sumParser <span class="fu">=</span> parser1 <span class="fu">&lt;||&gt;</span> parser2</code></pre>
</section>
<section class="slide">
<h2 id="remark-2">Remark (2)</h2>
<p>Similar structure:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">MyType</span> <span class="fu">=</span> <span class="dt">X</span> <span class="dt">Int</span> <span class="dt">Char</span> <span class="fu">|</span> <span class="dt">Y</span> <span class="dt">String</span>

parseMyType <span class="fu">=</span>      (<span class="dt">X</span> <span class="fu">&lt;$&gt;</span> parseInt <span class="fu">&lt;*&gt;</span> parseChar) 
              <span class="fu">&lt;||&gt;</span> (<span class="dt">Y</span> <span class="fu">&lt;$&gt;</span> parseString)</code></pre>
</section>
<section class="slide">
<h2 id="remark-3">Remark (3)</h2>
<p>Similar structure:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">Answer</span> <span class="fu">=</span> <span class="dt">YES</span> <span class="fu">|</span> <span class="dt">NO</span> <span class="fu">|</span> <span class="dt">MAYBE</span>
<span class="kw">data</span> <span class="dt">MyType</span> <span class="fu">=</span> <span class="dt">X</span> <span class="dt">Int</span> <span class="dt">Answer</span> <span class="fu">|</span> <span class="dt">Y</span> <span class="dt">String</span>

parseAnswer <span class="fu">=</span> parseYES <span class="fu">&lt;||&gt;</span> parseNO <span class="fu">&lt;||&gt;</span> parseMAYBE
parseMyType <span class="fu">=</span>      (<span class="dt">X</span> <span class="fu">&lt;$&gt;</span> parseInt <span class="fu">&lt;*&gt;</span> parseAnswer) 
              <span class="fu">&lt;||&gt;</span> (<span class="dt">Y</span> <span class="fu">&lt;$&gt;</span> parseString)</code></pre>
</section>
<section class="slide">
<h2 id="conclusion">Conclusion</h2>
<p>So Parser are more powerful than regular expression.<br />Parsec make it very easy to use.<br />Easy to read and to manipulate.</p>
<p>Notice how you could use parser as any other object in Haskell.<br />You could <code>mapM</code> them for example.</p>
<p>Any question?</p>
</section>
<section class="slide">
<h2 id="appendice-do-it-yourself">Appendice (Do it yourself)</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">{-# LANGUAGE DeriveDataTypeable #-}</span>
<span class="kw">import</span> Control.Applicative <span class="kw">hiding</span> (many, (&lt;|&gt;))
<span class="kw">import</span> Text.Parsec
<span class="kw">import</span> Data.Typeable
<span class="kw">type</span> <span class="dt">Parser</span> a <span class="fu">=</span> <span class="dt">Parsec</span> <span class="dt">String</span> () a

myparser <span class="fu">=</span> many1 letter

<span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span> test myparser <span class="st">&quot;myparser&quot;</span> <span class="st">&quot;(my (string to) parse)&quot;</span>

<span class="ot">test ::</span> (<span class="dt">Typeable</span> a, <span class="kw">Show</span> a) <span class="ot">=&gt;</span> <span class="dt">Parser</span> a <span class="ot">-&gt;</span> <span class="dt">String</span> <span class="ot">-&gt;</span> <span class="dt">String</span> <span class="ot">-&gt;</span> <span class="dt">IO</span> ()
test parser description string <span class="fu">=</span> <span class="kw">do</span>
    <span class="fu">putStrLn</span> <span class="fu">$</span> <span class="st">&quot;-- &quot;</span> <span class="fu">++</span> description <span class="fu">++</span> <span class="st">&quot; on \&quot;&quot;</span> <span class="fu">++</span> string <span class="fu">++</span> <span class="st">&quot;\&quot;&quot;</span>
    <span class="kw">let</span> res <span class="fu">=</span> parse parser description string
    <span class="kw">case</span> res <span class="kw">of</span>
        <span class="kw">Left</span>  err   <span class="ot">-&gt;</span> <span class="fu">print</span> err
        <span class="kw">Right</span> value <span class="ot">-&gt;</span> <span class="fu">putStrLn</span> <span class="fu">$</span> <span class="fu">show</span> value <span class="fu">++</span> <span class="st">&quot; :: &quot;</span> <span class="fu">++</span> <span class="fu">show</span> (typeOf value)</code></pre>
</section>
<section class="slide">
<h2 id="appendice-2">Appendice (2)</h2>
<p>Links to example code:</p>
<ul>
<li><a href="https://yogsototh.github.io/parsec-presentation/parsec/examples/01.hs">examples1.hs</a></li>
<li><a href="https://yogsototh.github.io/parsec-presentation/parsec/examples/scheme.hs">scheme.hs</a></li>
</ul>
</section>
<!-- End slides. -->
