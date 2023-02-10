# Contributing to Idris-Mode

Contributions are always welcome to `idris-mode`.
Here we describe how you can best contribute to `idris-mode`.

Much like Idris itself, `idris-mode` is run by volunteers and there are no full-time maintainers.
Our time is limited and we must take care to ensure that the demands of our day job take priority.
Thus, we must take care in ensure that *we* can maintain `idris-mode` itself.

## Tests

Before sending a patch or pull request, please run the automated tests for `idris-mode` and correct any errors that are found. There are two kinds of test:

1. The Emacs byte code compiler can catch many issues. Running `make compile` will invoke the byte code compiler, failing if there are any warnings. You may wish to run `make clean` after `make compile` to get rid of pesky `.elc` files.

2. There is a test suite that can be invoked with `make test`. It requires a functioning `idris` executable.

## General Comments

We expect contributions to come in the form of PRs (via GitHub), and larger discussions to take place on the project's issue tracker, the Idris Mailing List, or the Idris Discord.
While `idris-mode` does not pertain to any general philosophy we make one thing clear:

> First and foremost Idris-mode was created to provide a rich interacting editing experience in emacs for the language's created as part of the Idris project that support the IDE-Protocol.

Thus `idris-mode` must support *all* versions of the protocol associated with Idris1 and Idris2.

The `idris-mode` has simple test suite and some documentation.
Please try to ensure that contributions to the project keeps those in sync.

We do not prescribe to any coding style wrt to emacs lisp; this may change.
Please try to remember to keep the code styling consistent with how you found it.
A good rule to follow is that of the 'CampSite Rule': Leave the code tidier than you found it.

Please remember to update `CHANGELOG.md`, and if it's your first contribution you can add yourself to `CONTRIBUTORS`.

When submitting a pull request to the project, please try to adhere to good commit practises.

+ Lead the commit with a short description that explains it purpose;
+ optionally prepending the description with descriptive tags enclosed in square brackets;
+ If necessary provide a longer description after the brief.

GitHub will populate the description of a PR with the HEAD's commit message.
While convenient it may mean that the entire purpose of the PR is lost.
Please try to ensure that the PR description sufficiently describes the PR and not just the HERAD commit.
Similarly, try to ensure that the GitHub description does not contain information that should be in the commits.
We may end up moving from GitHub and we want to retain as much of the development history as possible.

In general, any major change to `idris-mode` should be discussed via the issue tracker and then resolved either by a PR or closing the issue.
We want to ensure that people do not waste their time on things we will not accept.
We will endeavour to keep track in the code-base larger discussions that are closed.

## Things we welcome

+ New features;
+ Fixes for the issue tracker;
+ Support for Version 2 of the IDE-Protoccol;
+ Documentation;
+ More tests;

## Things we want to discuss first

+ major code refactorings;
+ changes to the core operations;
+ anything that makes deep changes to how `idris-mode` operates;

## Things we are hesitent about

+ minor refactorings, reviewing PRs take time and minor refactorings based on style are not an effective use of a reviewers time, that being said if the refactorings are part of a larger PR then please do so, and make sure the refactorings are separated from the PRs main contribution;

## Things we will not welcome

+ Dropping of support for older versions of the protocol;


## Other possible contributions

If you have any other ideas on how to improve `idris-mode`, that is not covered above, please do get in touch.
We can discuss your idea and see how well it would fit in to the project as a whole.

## Where to talk.

The maintainers of `idris-mode` are embedded within the larger Idris project, you can find us hanging around there.
Good places to discuss possible contributions are:

    The mailing list <https://groups.google.com/forum/#!forum/idris-lang>_.
    The Idris community on Discord (Invite link) <https://discord.gg/YXmWC5yKYM>_
    The issue tracker (in this case, please make your proposal as concrete as possible).

<!-- EOF -->
