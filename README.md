# Advent of Code 2023 (in Haskell)

[Advent of Code](https://adventofcode.com) is an Advent calendar of small programming puzzles for a variety of skill sets and skill levels that can be solved in any programming language you like. People use them as a speed contest, interview prep, company training, university coursework, practice problems, or to challenge each other.

I'd like to get more familiar with the [Haskell](https://www.haskell.org) language, so I am using it to solve the AoC puzzles.
<br /> 
<br /> 

## Setup
Make sure [Docker](https://www.docker.com/products/docker-desktop) and [Visual Studio Code](https://code.visualstudio.com) are installed. Add the [Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) extension to Visual Studio Code. Clone the repository and open it in Visual Studio Code. No other installation needed.
<br /> 
<br /> 

## Running the code
```bash
cabal run aoc2023  # runs the code for all days

cabal run aoc2023 -- <day> # runs the code for the specific day (1-25)
```
<br /> 

## Version Info

```
The Glorious Glasgow Haskell Compilation System, version 9.2.7
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library 
stack Version 2.9.3, Git revision 6cf638947a863f49857f9cfbf72a38a48b183e7e aarch64 hpack-0.35.1
Platform Info:
  Linux b76298aa25bb 5.15.49-linuxkit #1 SMP PREEMPT Tue Sep 13 07:51:32 UTC 2022 aarch64 GNU/Linux
```

```bash
ghc --version
cabal --version
stack --version
uname -a 
```
