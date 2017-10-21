#INCLUDES=/home/pbg/projects/ocaml
OCAMLC=ocamlopt.opt # can change to ocamlopt.opt if you have it 
# Important: order is important, i.e. 1st module shouldn't depend on any other.
MODULES=str.cmxa unix.cmxa
BINARIES=vis cleanse split sample avt2evt evt2avt

all: $(BINARIES)

vis: vis.ml util.ml trace.ml
	$(OCAMLC) -o vis $(MODULES) util.ml trace.ml vis.ml

cleanse: cleanse.ml util.ml trace.ml
	$(OCAMLC) -o cleanse $(MODULES) util.ml trace.ml cleanse.ml

split: split.ml util.ml trace.ml
	$(OCAMLC) -o split $(MODULES) util.ml trace.ml split.ml

sample: sample.ml util.ml trace.ml
	$(OCAMLC) -o sample $(MODULES) util.ml trace.ml sample.ml

avt2evt: avt2evt.ml util.ml trace.ml
	$(OCAMLC) -o avt2evt $(MODULES) util.ml trace.ml avt2evt.ml

evt2avt: evt2avt.ml util.ml trace.ml
	$(OCAMLC) -o evt2avt $(MODULES) util.ml trace.ml evt2avt.ml

# Make an ocaml interpreter `shell' with the appropriate libraries included.
# Likely useful for OCaml programmers only.
top:
	ocamlmktop -o top str.cma unix.cma util.ml trace.ml

clean:
	rm -f $(BINARIES) *.cmi *.cmx *.o *.cmo top

