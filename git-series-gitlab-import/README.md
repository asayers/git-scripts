`git series-gitlab-import`
==========================

Get your private token from http://gitlab.example.com/profile/account

You need to set the following:

```
git config --global --add gitlab.url "http://gitlab.example.com"
git config --global --add gitlab.privateToken "XXXXXXXXXXXXXXXXXXXX"
git config --local  --add gitlab.projectId 1
```

or configure by environment variables:

```
export GITLAB_URL="http://gitlab.example.com"
export GITLAB_PRIVATE_TOKEN="XXXXXXXXXXXXXXXXXXXX"
export GITLAB_PROJECT_ID="1"
```

## Caveats

- It's expected that the git repo corresponds to the project you're importing
  from.
- In particular, if you try to import MRs from multiple projects into a single
  repo, you'll probably get collisions in terms of branch names.


## Use case: code review

Your colleague has opened an MR on gitlab (!1234, say) and has asked you to do
a code review. First, you sync gitlab's MRs to your local git repo:

```
$ git series-gitlab-import
!1234 (origin/my-awesome-branch): Created git-series/gitlab/1234
```

Then you checkout the series and review the code:

```
$ git series checkout gitlab/1234
$ git show SHEAD:cover
[...]
$ git log --oneline SHEAD:base..SHEAD:series
[...]
$ git diff --stat SHEAD:base..SHEAD:series
[...]
$ git series format --stdout
[etc.]
```

You make some comments on the MR by email or using the web interface, and your
colleague fixes the issues you indentified. They push the new code to the
branch tracked by !1234 and ask you to take another look.

```
$ git series-gitlab-import
!1234 (origin/my-awesome-branch): Updated git-series/gitlab/1234
$ git series checkout gitlab/1234
$ git series log -p
[...]
```

The changes look good, and you give your colleage a hearty "LGTM".

It might be a good idea to add a note the current revision of the series, so
that if your colleage makes some more changes to the branch in the future, you
can see which revision you signed off on:

```
$ git notes add -m "Signed-off-by: me" SHEAD
```

Unfortunately `git series log` doesn't show notes (yet), so this is not
particularly useful right now.

## More in-depth walkthrough

```
$ export GITLAB_URL="http://gitlab.example.com"
$ export GITLAB_PRIVATE_TOKEN="XXXXXXXXXXXXXXXXXXXX"
$ git series-gitlab-import 68
!1707 (origin/base.115-xxxxxxxxxxxx): Created git-series/gitlab/1707
!1706 (origin/base.115-xxxxxx): Nothing to do
!1705 (origin/base.115-xxxxxxxxxxxxx): Nothing to do
!1704 (origin/base.115-xxxx): Updated git-series/gitlab/1704
!1701 (origin/base.114-xxxxxx): Nothing to do
!1697 (origin/base.114-xxxxxxxx): Updated git-series/gitlab/1697
!1690 (origin/base.114-xxxxx): Updated git-series/gitlab/1690
!1678 (origin/base.113-xxxxxxxxxxxxxx): Nothing to do
!1675 (origin/base.113-xxxxxxxxxxxxxxxxxxxxx): Nothing to do
!1668 (origin/base.113-xxxxxxxxxxxxxx): Nothing to do
!1664 (origin/base.112-xxxxxxxxxxxxxxxxxxxxx): ERROR: Couldn't resolve object
!1643 (origin/base.111-xxxxxxxxxxxxxxxxxxxx): Nothing to do
!1639 (origin/base.112-xxxxxxxxxxxxxx): Nothing to do
```

Let's interpret the output:

- The MRs listed are the ones which are marked as "open" on gitlab (limited to
  the 100 most recently updated);
- !1707 was a new MR, which now exists as the series gitlab/1707;
- !1704, !1697, and !1690 were updated (either the description or the branch),
  and new commits were made on their respective branches containing those
  changes;
- !1664 threw an error - probably because the author deleted the target branch
  from the remote repository;
- the rest were already up-to-date.

Let's take a look at the kind of commits it generates:

```
$ git log -1 --pretty=fuller git-series/gitlab/1690
commit 52b75a59c8cc726d94020a60de7c95a95a2592c7
Author:     Alex Sayers <alex.sayers@gmail.com>
AuthorDate: Fri Nov 25 18:33:02 2016 +0900
Commit:     git-series-gitlab-import <>
CommitDate: Fri Nov 25 18:33:02 2016 +0900

    Pulled from http://gitlab.tsuru.it (!1690)
```

The author name is the name of the gitlab user who opened the MR. Since gitlab
doesn't tell us the user's email address, we try to figure it out from the git
repo. The commiter is "git-series-gitlab-import", who doesn't have an email
address. The timestamp is the current time.
