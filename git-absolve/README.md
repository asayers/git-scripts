git absolve
===========

Show the number of lines which were changed or removed since a given commit,
grouped by their former blamee.

```
Usage: git absolve <commit> [<commit>]
```

Suppose we want to find out who was absolved between master and
my-feature-branch.  The algorithm is effectively the following:

1. Take the diff between master and my-feature-branch.
2. For each line coloured red, run git blame on it.
3. Take the person who git blames and add 1 to their score.

For each person, we also show the timestamp of their most recent line.

This is meant to give an indication of whose code it is that my-feature-branch
is messing with. Perhaps you should consider CCing the top-scorers when you
start the code-review?

Example output (finding out who was absolved between the tag "1.1.0" and HEAD):

```
$ git absolve 1.1.0
    349   2016-10-17   Magnus Edenhill
     34   2015-12-22   François Saint-Jacques
      1   2015-02-18   Vaclav Krpec
```

This means that, since 1.1.0, the current branch has removed or modified 349
lines which were last touched by Magnus (the most recent of which he changed
on 17th Oct). I suppose he'd make a good reviewer.

Prior art
---------

This is a partial clone of Tim Pettersen's [git-guilt][1] (and it's [not even
the first clone][2]). Read about the design and intended use-case of
`git-guilt` [here][3]. Tim's original version is superior to the one found here
in a few ways:

- `git-guilt` tracks lines added as well as lines removed. I think this is a
  more robust algorithm than the one I use. (OTOH, it slows things down and
  doesn't make much of a difference when the branch you're diff-blaming has
  only a single author.)
- `git-guilt` displays results in a nice diffstatty format which looks better
  than `git absolve`.
- `git-guilt` has lots of options.

Why might you prefer this implementation? The original `git-guilt` is 300+
lines of javascript with 7 dependencies which have to be installed globally via
npm. When I tried running it on a branch, it took 1.6s to compute the result.
The python port is 800+ lines long, and took 3s to run on the same branch. `git
absolve` is 50 lines of sh and awk which takes 0.7s to run on that same branch.

Two other very similar programs are [git-related][4] and [git-contacts][5].
These seem to have slightly different behaviour from git-guilt and
git-absolve, but are clearly intended for the same purpose. They are very slow.

[1]: https://bitbucket.org/tpettersen/git-guilt/
[2]: https://github.com/mattboyer/git-guilt
[3]: http://blogs.atlassian.com/2014/07/git-guilt-blame-code-review/
[4]: https://github.com/felipec/git-related
[5]: https://github.com/git/git/tree/master/contrib/contacts
