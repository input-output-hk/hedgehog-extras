allowed-tools: [Bash(mkdir *), Read, Write, Edit, "omni-dev *"]
argument-hint: [range]
description: Twiddle git commit messages
model: claude-sonnet-4

# Steps

## Step 1
Run this command:

```bash
omni-dev git commit message view $ARGUMENTS
```

## Step 2
Analyse the result.  The result of the previous command is self describing.

## Step 3
Craft new commit messages for each commit and overwrite them to `.ai/scratch/amendments-<random-hash>.yaml`.

Where `<random-hash>` is a random hexadecimal hash of length 8.

Assume the `.ai/scratch` direcotry exists and try creating the directory if writing the file fails.

The file must conform to the following schema (validation required)
```yaml
amendments:                    # required, non-empty array
  - commit: "<40-hex-sha>"     # required, exactly 40 lowercase hex
    message: |                 # required; Conventional Commit
      <subject line>
      
      <wrapped body at 72 cols>
      
      <optional footers>
```

## Step 4
Run this command:

```bash
omni-dev git commit message amend .ai/scratch/amendments-<random-hash>.yaml
```

# Troubleshooting
If the `omni-dev` tool is not installed, then install this: https://crates.io/crates/omni-dev (at least `v0.5.0`).
