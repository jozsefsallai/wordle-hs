# wordle-hs

A terminal-based reimplementation of [Wordle][wordle-url] written in Haskell.

If you want to play a less primitive version of Wordle in the command line, you
can try [wordle-cli][wordle-cli-url].

## Demo

[![asciicast](https://asciinema.org/a/iJrbcLJKYLeTOE0iUVnwksZBc.svg)](https://asciinema.org/a/iJrbcLJKYLeTOE0iUVnwksZBc)

## Requirements

- git
- [Haskell][haskell-url]
- [Cabal][cabal-url]
- [Stack][stack-url]

You may use [GHCup][ghcup-url] to install Haskell and its tools more easily.

## Getting Started

You can download nightly builds [here][nightly-url]. Alternatively, you may
clone the repository and build the app yourself.

**1. Clone the repo:**

```
git clone git@github.com:jozsefsallai/wordle-hs.git
cd wordle-hs
```

**2. Build the project using Stack:**

```
stack build
```

**3. Run the executable:**

```
stack run
```

[wordle-url]: https://www.nytimes.com/games/wordle/index.html
[wordle-cli-url]: https://github.com/nimblebun/wordle-cli
[haskell-url]: https://www.haskell.org/
[cabal-url]: https://www.haskell.org/cabal/
[stack-url]: https://docs.haskellstack.org/en/stable/README/
[ghcup-url]: https://www.haskell.org/ghcup/
[nightly-url]: https://nightly.link/jozsefsallai/wordle-hs/workflows/release/master
