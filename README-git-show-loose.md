git show-loose
==============

Shows the loose blobs in the current repo, sorted by the time they were
created, and makes an effort to figure out the paths they appeared at.

```
usage: git show-loose (-p | --raw)

    -p  --paths   Try to give context for blobs by locating them in trees
        --raw     Produce more machine-readable output
```

What are "loose blobs"?
-----------------------

Suppose you stage some changes by `git add`ing a file to the index. Behind the
scenes, git takes a copy of the file and puts it in ".git/objects". The copy of
a file which lives inside ".git" is referred to as a "blob". Later, if you
decide to `git commit` your changes, git will create a commit which references
the blob, and it will be kept forever. If, on the other hand, you do `git
reset` and throw the changes away, the blob will eventually get deleted (the
next time git runs its garbage collector).

"Loose" objects are files in ".git/objects" which haven't yet been compressed
into a packfile (this is also done by the garbage collector). The details
aren't really important, but we can think of loose blobs as ones which git has
only recently copied into ".git".

Why might I want to see them?
-----------------------------

If something horrible happens and you lose your work, it may still be
recoverable from the ".git" directory. If you actually committed your changes
but lost them somehow (eg. during a rebase), there's a good chance `git reflog`
will help you find them.

However, if you only *staged* your changes but never actually committed them,
git doesn't offer many tools to help you. That's where `git show-loose` comes
in.

How do I use it?
----------------

Suppose I'm working on some changes to "Serialise.hs". I'm happy with the state
of things, so I add my files to the index ready to commit. At this point I go
and make a cup of coffee and everything gets swapped out of my brain.

When I come back I've had some new ideas about how the serialiser should work,
so I start making changes. I make a commit and immediately realise that the new
design is just coffee-fueled madness. No, it was definitely better how it was,
but no big deal: I'll just move back to `HEAD^`.

Now, of course, the tragic conclusion: `HEAD^` contains yesterday's
"Serialise.hs". The changes I made before visiting the coffee machine are gone.
At this point, I could begrudgingly reimplement them, or...

```
$ git show-loose --paths
2016-10-05 15:39:44     blob b9053aa5025dac0bd50aefd5e4fb6af2b4ae53ab (2975 bytes)      Benchmarks/JournalLogger.hs JournalLogger.hs src/Benchmarks/JournalLogger.hs
2016-10-05 15:39:44     blob 8a1a2afea9fa764cc40bd57cde52fac2bcd08af3 (2194 bytes)      Extras.hs Haskakafka/Extras.hs src/Haskakafka/Extras.hs
2016-10-05 15:39:44     blob 5b90cd167114ee43172c728d87d55d627d54db9f (8125 bytes)      Backend.hs Journal/Backend.hs src/Journal/Backend.hs
2016-10-05 15:39:43     blob e82fa32bf406004ce32a498c29ad206c5f1d2c80 (11623 bytes)     src/Tests/Unit.hs Tests/Unit.hs Unit.hs
2016-10-05 15:39:43     blob de6fdc185b591b9a641c8b055f63493fcc940317 (6903 bytes)      Journal/Serialise.hs Serialise.hs src/Journal/Serialise.hs
2016-10-05 15:39:43     blob dd076c48b00760bd78ed5ff5b82b0a2d2a15b6c1 (7566 bytes)      Backend.hs Journal/Backend.hs src/Journal/Backend.hs
2016-10-05 15:39:43     blob d7af7e1a3dbc4f7688d277555e0ce6c7d72a9ac3 (1486 bytes)      Extras.hs Haskakafka/Extras.hs src/Haskakafka/Extras.hs
2016-10-05 15:39:43     blob c61485634b021c4d1de37e1973d3178f72d64373 (6971 bytes)      Journal/Logger.hs Logger.hs src/Journal/Logger.hs
...
```

As you can see, `git show-loose` has displayed all the loose blobs in the
current git repo, ordered with the most recently created ones at the top.
(Also, because I passed `--paths`, it looked at the loose trees to try to
figure out what those blobs are called).

I can now take a look at the contents of those blobs using `git cat-file -p`.
After looking through a few, I find "Serialise.hs" in its pre-caffeinated
state. Hurrah! I saved 15 minutes compared to reimplementing my changes, and I
now proceed to waste 15 collective minutes of my colleagues' time bragging
about how I averted disaster with my git mastery.
