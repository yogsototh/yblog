-----
isHidden:       false
menupriority:   1
kind:           article
published:     2010-05-24T20:05:14+02:00
title: Trees; Pragmatism and Formalism
subtitle: When theory is more efficient than practice
authorName: Yann Esposito
authorUri: yannesposito.com
tags: XML, Perl, programming, tree, theory, mathematics, regexp, script
-----

<div class="intro">

%tldr: 

- I tried to program a simple filter
- Was blocked 2 days
- Then stopped working like an engineer monkey
- Used a pen and a sheet of paper
- Made some math.
- Crushed the problem in 10 minutes
- Conclusion: The pragmatism shouldn't mean "never use theory".

</div>

## Abstract (longer than %tldr)

For my job, I needed to resolve a problem. It first seems not too hard. 
Then I started working directly on my program. 
I entered in the *infernal*: *try &amp; repair loop*.
Each step was like:

>   -- Just this thing to repair and that should be done.  
>   -- OK, now that should just work.  
>   -- Yeah!!!  
>   -- Oops! I forgotten that...  
>   `repeat until death`

After two days of this [Sisyphus](http://fr.wikipedia.org/wiki/Sisyphe) work, I finally just stopped to rethink the problem.
I took a pen, a sheet of paper. I simplified the problem, reminded what I learned during my Ph.D. about trees.
Finally, the problem was crushed in less than 20 minutes.

I believe the important lesson is to remember that the most efficient methodology to resolve this *pragmatic* problem was the *theoretical* one. 
And therefore, argues opposing science, theory to pragmatism and efficiency are fallacies.



# First: my experience

Apparently 90% of programmer are unable to program a binary search without bug. 
The algorithm is well known and easy to understand. 
However it is difficult to program it without any flaw. 
I participated to [this contest](http://reprog.wordpress.com/2010/04/19/are-you-one-of-the-10-percent/).
And you can see the [results here](http://reprog.wordpress.com/2010/04/21/binary-search-redux-part-1/)[^1].
I had to face a problem of the same kind at my job. The problem was simple to the start. Simply transform an <sc>xml</sc> from one format to another.

[^1]: Hopefully I am in the 10% who had given a bug free implementation.

The source <sc>xml</sc> was in the following general format:

<code class="xml">
<rubrique>
    <contenu>
        <tag1>value1</tag1>
        <tag2>value2</tag2>
        ...
    </contenu>
    <enfant>
        <rubrique>
            ...
        </rubrique>
        ...
        <rubrique>
            ...
        </rubrique>
    </enfant>
</menu>
</code>

and the destination format was in the following general format:

<code class="xml">
<item name="Menu0">
    <value>
        <item name="menu">
            <value>
                <item name="tag1">
                    <value>value1</value>
                </item>
                <item name="tag2">
                    <value>value2</value>
                </item>
                ...
                <item name="menu">
                    <value>
                        ...
                    </value>
                    <value>
                        ...
                    </value>
                </item>
            </value>
        </item>
    </value>
</item>
</code>

At first sight I believed it will be easy. I was so certain it will be easy that I fixed to myself the following rules:

1. do not use <sc>xslt</sc>
2. avoid the use of an <sc>xml</sc> parser
3. resolve the problem using a simple perl script[^2]

You can try if you want. If you attack the problem directly opening an editor, I assure you, it will certainly be not so simple.
I can tell that, because it's what I've done. And I must say I lost almost a complete day at work trying to resolve this. There was also, many small problems around that make me lose more than two days for this problem.

Why after two days did I was unable to resolve this problem which seems so simple?

What was my behaviour (workflow)?

1. Think
2. Write the program
3. Try the program 
4. Verify the result
5. Found a bug
6. Resolve the bug
7. Go to step 3.

This was a *standard* workflow for computer engineer. The flaw came from the first step. 
I thought about how to resolve the problem but with the eyes of a *pragmatic engineer*. I was saying:

> That should be a simple perl search and replace program.  
> Let's begin to write code

This is the second sentence that was plainly wrong. I started in the wrong direction. And the workflow did not work from this entry point.

## Think

After some times, I just stopped to work. Tell myself *"it is enough, now, I must finish it!"*.
I took a sheet of paper, a pen and began to draw some trees.

I began by make by removing most of the verbosity.
I first renamed `<item name="Menu">` by simpler name `M` for example.
I obtained something like:

<graph title="The source tree">
    subgraph cluster_x {
        node [label="C"] C_x ;
        node [label="E"] E_x ;
        node [label="a1"] tag1_x ;
        node [label="a2"] tag2_x ;
        node [label="R", color="#333333", fillcolor="#333333", fontcolor="white"] R_x ;
        R_x -> C_x;
        C_x -> tag1_x ;
        C_x -> tag2_x ;
        R_x -> E_x ;
    }
    subgraph cluster_y {
        node [label="C"] C_y ;
        node [label="E"] E_y ;
        node [label="a1"] tag1_y ;
        node [label="a2"] tag2_y ;
        node [label="R", color="#333333", fillcolor="#333333", fontcolor="white"] R_y ;
        R_y -> C_y;
        C_y -> tag1_y ;
        C_y -> tag2_y ;
        R_y -> E_y ;
    }
    subgraph cluster_z {
        node [label="C"] C_z ;
        node [label="E"] E_z ;
        node [label="a1"] tag1_z ;
        node [label="a2"] tag2_z ;
        node [label="R", color="#333333", style="filled", fillcolor="#333333", fontcolor="white"] R_z ;
        R_z -> C_z;
        C_z -> tag1_z ;
        C_z -> tag2_z ;
        R_z -> E_z ;
    }
    E_x -> R_y ;
    E_x -> R_z ;

</graph>

and

<graph title="The destination tree">
    subgraph cluster_x {
        node [label="M"] E_x ;
        node [label="a1"] tag1_x ;
        node [label="V"] value_tag1_x ;
        node [label="a2"] tag2_x ;
        node [label="V"] value_tag2_x ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_x ;
        R_x -> value_tag1_x -> tag1_x ;
        R_x -> value_tag2_x -> tag2_x ;
        R_x -> E_x ;
    }
    subgraph cluster_y {
        node [label="M"] E_y ;
        node [label="a1"] tag1_y ;
        node [label="V"] value_tag1_y ;
        node [label="a2"] tag2_y ;
        node [label="V"] value_tag2_y ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_y ;
        R_y -> value_tag1_y -> tag1_y ;
        R_y -> value_tag2_y -> tag2_y ;
        R_y -> E_y ;
    }
    subgraph cluster_z {
        node [label="M"] E_z ;
        node [label="a1"] tag1_z ;
        node [label="V"] value_tag1_z ;
        node [label="a2"] tag2_z ;
        node [label="V"] value_tag2_z ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_z ;
        R_z -> value_tag1_z -> tag1_z ;
        R_z -> value_tag2_z -> tag2_z ;
        R_z -> E_z ;
    }
    E_x -> R_y ;
    E_x -> R_z ;

</graph>

Then I made myself the following reflexion:

Considering Tree Edit Distance, each unitary transformation of tree correspond to a simple search and replace on my <sc>xml</sc> source[^nb].
We consider three atomic transformations on trees:

 - *substitution*: renaming a node
 - *insertion*: adding a node
 - *deletion*: remove a node

[^nb]: I did a program which generate automatically the weight in a matrix of each edit distance from data.

One of the particularity of atomic transformations on trees, is ; if you remove a node, all children of this node, became children of its father.

An example:

<pre class="twilight">
r - x - a
  \   \
   \    b
    y - c   
</pre>

If you delete the `x` node, you obtain

<pre class="twilight">
    a
  /
r - b
  \
    y - c   
</pre>

And look at what it implies when you write it in <sc>xml</sc>:

<code class="xml">
<r>
  <x>
    <a>value for a</a>
    <b>value for b</b>
  </x>
  <y>
    <c>value for c</c>
  </y>
</r>
</code>

Then deleting all `x` nodes is equivalent to pass the <sc>xml</sc> via the following search and replace script:

<code class="perl">
s/<\/?x>//g
</code>

Therefore, if there exists a one state deterministic transducer which transform my trees ;
I can transform the <sc>xml</sc> from one format to another with just a simple list of search and replace directives.

# Solution

Transform this tree:

<pre class="twilight">
R - C - tag1
  \   \
   \    tag2
    E -- R - C - tag1
      \   \    \
       \   \     tag2
        \    E ...
         R - C - tag1 
           \    \
            \     tag2
             E ...
</pre>

to this tree:

<pre class="twilight">
                tag1
              /
M - V - M - V - tag2      tag1
              \         / 
                M --- V - tag2
                  \     \ 
                   \      M
                    \     tag1
                     \  / 
                      V - tag2
                        \ 
                          M
</pre>

can be done using the following one state deterministic tree transducer:

>    C -> &epsilon;  
>    E -> M  
>    R -> V  

Wich can be traduced by the following simple search and replace directives: 

<code class="perl">
s/C//g
s/E/M/g
s/R/V/g
</code>

Once adapted to <sc>xml</sc> it becomes:

<code class="perl">
s%</?contenu>%%g
s%<enfant>%<item name="menu">%g
s%</enfant>%</item>%g
s%<rubrique>%<value>%g
s%</rubrique>%</value>%g
</code>

That is all.

# Conclusion

It should seems a bit paradoxal, but sometimes the most efficient approach to a pragmatic problem is to use the theoretical methodology.
