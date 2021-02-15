#CONFIG=$(CPA)/config/predicateAnalysis-PredAbsRefiner-ABEl.properties
#CONFIG=$(CPA)/config/default.properties
#OPTIONS=-setprop cinvariants.export=true -setprop cinvariants.prefix=dumpInv -setprop solver.solver=Z3

all: cvv

cvv: cvv.ml
	ocamlc cvv.ml -o cvv

memory: cvv
	
cpamac:
	BENCHES=`./cityprover --benches=m,ac,c`
	./cityprover --verifier=cpa --stages=oneshot,rv,oeA,oeB ${BENCHES} 

cpault:
	BENCHES=`./cityprover --benches=m,ac,c`
	./cityprover --verifier=ult --stages=oneshot,rv,oeA,oeB ${BENCHES} 
