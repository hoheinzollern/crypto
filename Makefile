SHA_DIR=$(shell ocamlfind query sha)

OCAML_COMP=ocamlopt

hmac: HMAC.ml HMAC.mli
	ocamlfind ocamlopt -package sha -linkpkg HMAC.mli HMAC.ml -o hmac
