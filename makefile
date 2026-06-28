default: build run clean

build: vbnet

vbnet: vbnet.ml
	ocamlc -c vbnet.ml
	ocamlc -o vbnet vbnet.cmo

run:
	./vbnet

clean:
	rm -rf vbnet vbnet.cm* 


