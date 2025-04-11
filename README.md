# Two Phase

An implementation of Two phase protocol (simplified) specification. 

- `spec/**`: contains the `TwoPhase` specification and trace specification
- `src/**`: contains the `TwoPhase` implementation

# Prerequisites

- Java >= 17
- Apache maven >= 3.6
- Python >= 3.9
- TLA+ >= 1.8.0 (The Clarke release)

### Install the trace validation tools (and TLA+)

See README at https://github.com/lbinria/trace_validation_tools

### Install python librairies

The `ndjson` Python library is needed in order to perform the
validation; it can be installed with:

`pip install ndjson`

We suppose that `python` and `pip` are the commands for Python and
its package installer, if otherwise you should change the above line
and some of the following accordingly.

# Build the Java program

Change the version of the dependency `org.lbee.instrumentation` in the
file [pom.xml](pom.xml) according to the one you use (in .m2 or on the
github maven registry) and run

`mvn package`

# Perform trace validation

To check the conformity of the trace produced by the program, the
script [trace_validation_pipeline.py](trace_validation_pipeline.py)
can be used:

`python trace_validation_pipeline.py -c`

It consists of the following steps:
- clean old trace files
- compile implementation of TwoPhase
- run implementation of TwoPhase
- merge trace files 
- Run TLC on the resulting trace file

### Perform trace validation on a trace file

Alternatively, we can run the implementation with the command

`python run_impl.py`

The default configuration file `conf.ndjson` used by `run_impl.py` declares two ressource managers; this can be changed in the file or by using another file (specified with `--config`).

Then, traces can be merged

`python trace_merger.py --sort True`

The same configuration file `conf.ndjson` is used by default (can be changed with `--config`). The files to be merged can be also specified explicitly as arguments for the script (if no file is pspecified, the list of files is extracted from the config file supposing that each line corresponds to a process that logs in a file named accordingly). By default the result of the merge is dumped in the file `trace.ndjson` (a different one can be specified with `--out`).

Finally, we can perform the trace validation on the obtained trace file `trace.ndjson` (a different one can be specified with `--trace`) with the configuration specified in the file `conf.ndjson` (a different one can be specified with `--config`) by using the command:

`python tla_trace_validation.py spec/TwoPhaseTrace.tla`

# Directory structure

- `spec/**`: contains the `TwoPhase` specification and trace specification
- `src/**`: contains the `TwoPhase` implementation
