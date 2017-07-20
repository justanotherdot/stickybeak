# stickybeak

![Closeup of a parrot's feathers](birds.jpg)


[![Build Status](https://travis-ci.org/justanotherdot/stickybeak.svg?branch=master)](https://travis-ci.org/justanotherdot/stickybeak)

A file watch and trigger tool written in Haskell

## Install

Presently only linux is supported, but in due time other platforms will hopefully
be supported as well. You'll need the inotify library which you can get by
following the instructions [here](https://github.com/rvoicilas/inotify-tools/wiki).

Next, clone the repository and build with stack.

```bash
git clone https://www.github.com/justanotherdot/stickybeak.git
cd stickybeak && stack build
```

## Example Usage

```bash
stickybeak "src" "runTests.sh"
```

To watch some source directory and run a test suite on changes.
