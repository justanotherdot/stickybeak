# stickybeak

[![Build Status](https://travis-ci.org/justanotherdot/stickybeak.svg?branch=master)](https://travis-ci.org/justanotherdot/stickybeak)

A file watch (and trigger) tool written in Haskell

## Install

```bash
git clone https://www.github.com/justanotherdot/stickybeak.git
cd stickybeak && stack build
```

## Usage

`stickybeak` runs in two modes: `watch` mode watches a single directory and runs
a single task on file changes inside that directory, e.g.

```
stickybeak watch src 'stack test'
```

additionally, `triggers` allows an explicit or implicit config file to be read
for multiple triggers. Config files have the format

```yaml
triggers:
  - name: Tests
    dirs: [., src]
    cmd: stack
    args: [test]
```

Which will set up a single set of triggers running `stack test` across "." and
"src" named "Tests". `stickybeak triggers` will search for a ".stickybeak.yaml"
file in the current working directory whereas `stickbeak triggers -c PATH`
will use the config located at PATH.
