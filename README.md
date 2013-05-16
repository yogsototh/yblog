# yannesposito.com Source code

I use Hakyll to generate my website.

If you want to use this blog for you.

1. Clone the branch 'clean' from my repository

        git clone http://github.com/yogsototh/yblog -b clean

2. Configure your languages in `Config.hs`
3. compile and launch preview

        ./preview

4. Add an avatar.png image in `Scratch/img/about`
5. You might want to change `Multilang.lhs` inside `trads` to put your own translation.
6. You might want to add some abbreviations time to time `Abbreviations.lhs`.
7. Create your own entries in `multi` (if you use different languages)
   or directly inside Scratch/
8. For google analytics, modify the identifier `UA-0000000-1` in `Scratch/js/index.js`
9. For the disqus commment modify the `disqus_shortname` value in `templates/post.html`.
10. To publish to github pages, modify the github conf inside `publish.sh` and `fastpublish.sh`.


## Workflow

1. Open a terminal and type `./preview`
2. edit content inside Scratch or multi
3. Once happy, `./publish.sh`.
