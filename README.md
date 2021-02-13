## Setup

### Prerequisites

- MSBuild
- sbt (Scala build tool)

### Installation

1. Create a new directory wherever you would like to store DafnyDoc and its dependencies, e.g. `mkdir dfy`.
2. Navigate to the directory you have just created, e.g. `cd dfy`.
3. Clone [this fork](https://github.com/nhweston/dfy) of the Dafny compiler using the following command: `git clone git@github.com:nhweston/dfy.git ./cmp --recurse-submodules`.
4. Navigate to the directory containing the Dafny compiler and run `msbuild Source/Dafny.sln`.
5. Clone this repository.
6. Navigate to the directory containing this repository and run `sbt assembly`.
7. Optionally, add the `bin` directory to your `PATH` so you can run `dafnydoc` from anywhere.

## Usage

Generate documentation for `program.dfy` (and all its includes) in directory `doc`:

```
dafnydoc -g program.dfy doc
```

Print documentation for `a.dfy` (just the single file):

```
dafnydoc -p a.dfy
```

Print the documentation tree for `program.dfy`:

```
dafnydoc -p a.dfy -t
```

…alternatively in JSON format:

```
dafnydoc -p a.dfy -j
```

Pass `-v` (verbose mode) to print output from the Dafny server.
