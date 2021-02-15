# CityProver Commutativity Verifier

## Installation from fresh Ubuntu 20.04

```
sudo apt-get update
sudo apt-get install git ocaml opam
git clone https://github.com/erickoskinen/cityprover.git
cd cityprover/src
make
```

## Adding a new ADT

 * Edit `cvv.ml`:
    * update `all_bench()` to add new `Bench` entries in the list. You will need to define `Method`s for the ADT's API. `Bench(bname,source,mlist,m,n,phi,I)` constructor consists of:
       * `bname`: A handy name for the benchmark
       * `source`: The C source filename (without `.c`) 
       * `mlist`: The list of all methods 
       * `m,n`: The two methods of consideration 
       * `phi`: The commutativity condition 
       * `I`: The observational equivalence relation. This will be an expression that is `assert()`ed and `assume()`ed.
    * update `s1_init_state` to provide the initial state;
    * update `assume_state_space` to provide a list of assumptions to constrain the state space to valid states
    * update `clone_post` to provide the "deep copy" of the state (ie "dup")
    * 
