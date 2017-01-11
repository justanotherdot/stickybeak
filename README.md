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


additionally, there is a second mode, `triggers`, which allows an explicit or
implicit config file to be read for multiple triggers. Given we have the
following file `stickybeak.yaml` in our current working directory with the contents:

```yaml
triggers:
  - dirs: [., src]
    cmd: stack
    args: [test]
```

running `stickybeak triggers` will watch both the current working directory
and the 'src' directory and run `stack test` whenever changes occur.
