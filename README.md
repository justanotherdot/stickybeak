# stickybeak

[![Build Status](https://travis-ci.org/justanotherdot/stickybeak.svg?branch=master)](https://travis-ci.org/justanotherdot/stickybeak)

A file watch (and trigger) tool written in Haskell

## Install

```bash
git clone https://www.github.com/justanotherdot/stickybeak.git
cd stickybeak && stack build
```

## Usage

`stickybeak` supports basic command line args in two modes. `watch` mode
watches a single directory and runs a single task on file changes inside that
directory.

```
stickybeak watch src 'stack test'
```


but the aim is to have a second mode `triggers` which allows an explicit or
implicit config file to be read for multiple triggers.

```yaml
triggers:
  - dirs: [., src]
    cmd: stack
    args: [test]
```
