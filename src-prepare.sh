#!/bin/sh

ocaml_version="$(ocamlc --version|sed - -e 's/\(4\.[0-9][0-9]\)\..*/\1/')"

case "${ocaml_version}" in
    4.11) true ;;
    4.10) patch -p1 -u < patches/ocaml.4.10.patch ;;
    4.09|4.08) patch -p1 -u < patches/ocaml.4.08-4.09.patch ;;
    *) echo "unsupported OCaml version ${ocaml_version}"; exit 1 ;;
esac
