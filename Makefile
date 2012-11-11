ps2.exe: streams.mli streams.ml more.ml ps2.ml
	ocamlc streams.mli streams.ml more.ml ps2.ml -o ps2.exe
