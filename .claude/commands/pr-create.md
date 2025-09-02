allowed-tools: [Bash(mkdir *), Read, Write, Edit, "omni-dev *"]
argument-hint: [range]
description: Twiddle git commit messages
model: claude-sonnet-4

# Steps

## Step 1
Run this command:

```bash
omni-dev git branch info
```

## Step 2
Analyse the result.  The result of the previous command is self describing.

In particular, try to find all the fields documented by `.explanation` in the result and print the fields that are relevant for PR creation.

Things to look for include, but are not limited to:

* Whether the working directory is clean
* Whether there existing PRs
* What branch PRs already exist this branch
* What the name of the current checked out branch is
* What the PR template text is if it exists

A thorough search is sometimes needed to find the field.

## Step 3
If according to the results, a PR for this branch already exists, then stop.

If according to the result there are untracked changes then stop.

If according to the result the PR needs to be rebased on the remote main branch, then stop.

If a PR for this branch doesn't yet exist, then create a PR with a PR description based on the PR template in the result.

To create the PR, use the Github CLI.

# Troubleshooting
If the `omni-dev` tool is not installed, then install this: https://crates.io/crates/omni-dev (at least `v0.5.0`).
