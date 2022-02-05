FROM ocaml/opam:alpine

RUN opam update && \
  opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git && \
  opam install -y\
  dune \
  ocaml-lsp-server \
  ocamlformat.0.20.1

# opam env
ENV OPAM_SWITCH_PREFIX /home/opam/.opam/4.13
ENV CAML_LD_LIBRARY_PATH /home/opam/.opam/4.13/lib/stublibs:/home/opam/.opam/4.13/lib/ocaml/stublibs:/home/opam/.opam/4.13/lib/ocaml
ENV OCAML_TOPLEVEL_PATH /home/opam/.opam/4.13/lib/toplevel
ENV MANPATH :/home/opam/.opam/4.13/man
ENV PATH $PATH:/home/opam/.opam/4.13/bin
