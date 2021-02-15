# CityProver Commutativity Verifier

## Installation from fresh Ubuntu 20.04

```
sudo apt-get update
sudo apt-get install git ocaml opam

# Ultimate
wget https://github.com/ultimate-pa/ultimate/releases/download/v0.1.25/UltimateAutomizer-linux.zip
unzip UltimateAutomizer-linux.zip 
# CPAchecker
sudo apt-get install openjdk-11-jdk
wget https://cpachecker.sosy-lab.org/CPAchecker-1.9.1-unix.zip
unzip CPAchecker-1.9.1-unix.zip

# Perl modules
cpan -i Time::Out  # Note: configure cpan with "local::lib"
export PERL5LIB=~/perl5/lib/perl5/

# Apache so you can view the HTML output
sudo apt-get install apache2
sudo mkdir /var/www/html/cityprover
sudo chmod 777 /var/www/html/cityprover


git clone https://github.com/erickoskinen/cityprover.git
cd cityprover/src
make
```

## Running CityProver

```
cd cityprover/src
export ULT_HOME=~/UAutomizer-linux
export CPA_HOME=~/CPAchecker-1.9.1-unix
./cityprover                                     # View options
./cityprover --bench                             # List available benchmarks
./cityprover --verifier=ult c-decr-decr-true-frv # run one benchmark
```

Once it completes, it will instruct you how to harvest the results, along the lines of:

```
 ./harvest --s=Rcpa,DARcpa c-decr-decr-true-frv
 ```

 This generates a LaTeX output. If you add `--latexmacros` it will surround this output with
 everything needed to compile the latex.


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
