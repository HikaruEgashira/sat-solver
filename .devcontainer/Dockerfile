FROM ocaml/opam2:latest

ENV DEBIAN_FRONTEND=noninteractive

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

USER root

RUN apt-get --allow-releaseinfo-change -o Acquire::Check-Valid-Until=false -o Acquire::Check-Date=false update \
  && apt-get -y install --no-install-recommends m4 \
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

USER opam

RUN opam update && \
  opam install \
  dune \
  merlin 

RUN opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git && \
  opam install ocaml-lsp-server

ENV DEBIAN_FRONTEND=dialog
