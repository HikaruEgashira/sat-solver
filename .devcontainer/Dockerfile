FROM ocaml/opam:alpine

COPY . /home/opam/workspace
RUN sudo chown opam -R /home/opam/workspace

WORKDIR /home/opam/workspace

RUN opam update && \
  opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git \
  opam install -y --deps-only \
  dune \
  ocaml-lsp-server \
  ocamlformat.0.20.1

RUN eval $(opam env)
# ENV PATH $PATH:/home/opam/.opam/4.13/bin
