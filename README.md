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
  - dirs: [., src]
    cmd: stack test
    recursive: true
```

The above config would set up triggers on the current working directoy,  the
'src' path, and all subdirectories below each. On file changes, `stack test`
will be run. One can specify a different `.stickybeak.yml` file by runnig
stickybeak as `stickybeak triggers -c 'some/other/config.yml'`
