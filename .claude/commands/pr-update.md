allowed-tools: [Bash(mkdir *), Read, Write, Edit, "omni-dev *"]
argument-hint: [range]
description: Update existing pull request
model: claude-sonnet-4

# Steps

## Step 1
Run this command:

```bash
omni-dev git branch info
```

## Step 2
Analyse the result.  The result of the previous command is self describing.

In particular, try to find all the fields documented by `.explanation` in the result and print the fields that are relevant for PR updates.

Things to look for include, but are not limited to:

* Whether the working directory is clean
* Whether there are existing PRs for this branch
* What the PR number, title, and current state are
* What the name of the current checked out branch is
* What the current PR description/body contains

A thorough search is sometimes needed to find the field.

## Step 3
If according to the results, no PR exists for this branch, then stop.

If according to the result there are untracked changes then stop.

If according to the result the PR needs to be rebased on the remote main branch, then stop.

If a PR for this branch exists, then get the current PR description and write it to a
backup files in .ai/scratch.  Update the PR title and description based on the latest
commits and changes in the branch.  The new PR description should conform to the PR template
if it exists and incorporate information from the previous PR description that could not be
inferred from the changes and are likely added by the user.  Feel free to check the code
outside the code changs in the branch for context.

Write the proposed PR title and PR description (as embedded markdown) to a yaml file in
the `.ai/scratch` directory.

## Step 4

Use the GitHub CLI to update the existing PR.

# Troubleshooting
If the `omni-dev` tool is not installed, then install this: https://crates.io/crates/omni-dev (at least `v0.5.0`).
