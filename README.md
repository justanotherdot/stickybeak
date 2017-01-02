# stickybeak

[![Build Status](https://travis-ci.org/justanotherdot/stickybeak.svg?branch=master)](https://travis-ci.org/justanotherdot/stickybeak)

A file watch (and trigger) tool written in Haskell

## Install

```bash
git clone https://www.github.com/justanotherdot/stickybeak.git
cd stickybeak && stack build
```

## Usage

The aim is to have `stickybeak` support a project config or command line args,
e.g. by calling

```
stickybeak -m . "stack test"
```

on the shell to automatically run `stack test` whenever any files in the current
directory are modified (NOTE: this specific usage is not set in stone).

Conversely, stickybeak, when finding no command line arguments, will look
for a `stickybeak.yaml` in the same directory as its running instance. If one is
found, `stickybeak` will setup triggers as specified, e.g.

```yaml
triggers:
  - dirs: [., src]
    cmd: stack
    args: [test]
```
