FROM debian:bullseye

RUN apt-get update
RUN apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN apt-get install -y libnuma-dev
RUN apt-get install -y git
RUN apt-get install -y zlib1g-dev


RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
ENV PATH="$PATH:/root/.local/bin:/root/.ghcup/bin:/root/.cabal/bin/:/root/.stack/bin/"gen-BOOTSTRAP_HASKELL_NONINTERACTIVE
RUN cabal update
RUN stack update
RUN ghcup install ghc 9.2.7
RUN ghcup install hls 2.0.0.0
RUN cabal install hlint

# RUN cabal install hoogle
# RUN echo >> ~/.ghci ':def hoogle \x -> return $ ":!hoogle \"" ++ x ++ "\""'
# RUN echo >> ~/.ghci ':def doc \x -> return $ ":!hoogle --info \"" ++ x ++ "\""'

# RUN hoogle generate
RUN mkdir /workspaces/aoc2023 -p

RUN stack update
RUN stack install haskell-dap ghci-dap haskell-debug-adapter

RUN cabal update
RUN cabal install ghci-dap haskell-debug-adapter

RUN ghcup set ghc 9.2.7
