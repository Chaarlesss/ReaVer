FROM ubuntu:22.04 AS dev
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y build-essential m4 autoconf curl git libgmp-dev libmpfr-dev opam wget && apt-get clean

WORKDIR /home/root/
RUN opam init --disable-sandboxing
RUN opam update
RUN opam switch create -y ./ --deps-only
RUN opam install -y dune ocaml-lsp-server ocamlformat user-setup
RUN eval $(opam env)

FROM dev
WORKDIR /workspaces/reaver/
COPY . /workspaces/reaver/
RUN dune build
ENTRYPOINT [ "dune", "exec", "reaver" ]