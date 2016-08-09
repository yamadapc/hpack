FROM haskell:7.8.4
ADD ./hpack-convert.cabal /hpack-convert/hpack-convert.cabal
WORKDIR /hpack-convert
RUN cabal update
RUN cabal sandbox init
RUN cabal install --only-dep -j
ADD . /hpack-convert
RUN cabal configure
RUN cabal build
