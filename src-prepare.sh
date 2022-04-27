#!/bin/sh

ocaml_version="$(ocamlc --version|sed - -e 's/\(4\.[0-9][0-9]\)\..*/\1/')"
coq_version="$(coqc --version|head -n1|sed - -e 's/.*\(8\.[0-9][0-9]\).*/\1/')"

case "${ocaml_version}" in
    4.13) true ;;
    4.11|4.12) patch -p1 -u < patches/ocaml.4.11-4.12.patch ;;
    4.10) patch -p1 -u < patches/ocaml.4.10.patch ;;
    4.09|4.08) patch -p1 -u < patches/ocaml.4.08-4.09.patch ;;
    *) echo "unsupported OCaml version ${ocaml_version}"; exit 1 ;;
esac

case "${coq_version}" in
    8.15 | 8.14) true ;; # dev versions
    8.13) true ;;
    8.12) patch -p1 -u < patches/coq.8.12.patch ;;
    *) echo "unsupported Coq version ${coq_version}"; exit 1 ;;
esac
