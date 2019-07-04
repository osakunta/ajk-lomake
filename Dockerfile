FROM haskell:8.4.4 as build-env

WORKDIR /build

RUN apt-get update && apt-get install -y liblzma-dev libghc-postgresql-libpq-dev

RUN cabal new-update

COPY ./cabal.project .
COPY ./ajk-lomake.cabal .

RUN cabal new-install --only-dependencies -j4

COPY . .

RUN cabal new-configure
RUN cabal new-build
