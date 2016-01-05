# emoji.yml

[![Build Status](https://travis-ci.org/siddharthist/emoji.yml.svg)](https://travis-ci.org/siddharthist/emoji.yml)

## description
emoji.yml is a machine-readable (yaml) list of (almost) all unicode emoji
characters, along with their names and hex codes.

## details
the source data is [a table given by the unicode consortium][table]. it has been
transformed using this simple haskell script to produce emoji.yml. like said
table, it doesn't include the modifier or zwj sequences.

the provided version doesn't include images, but there is a comment in the
source that explains how to include them in the output.

## installation
just put emoji.yml wherever you want! if you want to build it from source, you
can use `cabal run` like on any other haskell project.

[table]: http://unicode.org/emoji/charts/emoji-list.html
