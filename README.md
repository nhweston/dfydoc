## Setup

### Prerequisites

- MSBuild
- sbt (Scala build tool)

### Installation

#### Windows

1. Clone [this fork](https://github.com/nhweston/dfy) of the Dafny compiler.
2. Navigate to the directory containing the Dafny compiler and run `msbuild Source/Dafny.sln`.
3. Clone this repository.
4. Navigate to the directory containing this repository and run `sbt assembly`.
5. Navigate to the `bin` directory.
6. Open `dafnydoc.bat` in a text editor.
7. Set `DAFNYDOC_JAR` to the absolute path of `dafnydoc.jar` (in the `bin` directory).
8. Set `DAFNY_SERVER_PATH` to the absolute path of `DafnyServer.exe` in the `Binaries` subdirectory
   of the Dafny compiler.
9. Add the `bin` directory to your `PATH`.

#### Linux

1. Clone [this fork](https://github.com/nhweston/dfy) of the Dafny compiler.
2. Navigate to the directory containing the Dafny compiler and run `msbuild Source/Dafny.sln`.
3. Clone this repository.
4. Navigate to the directory containing this repository and run `sbt assembly`.
5. Navigate to the `bin` directory.
6. Open `dafnydoc.sh` in a text editor.
7. Set `DAFNYDOC_JAR` to the absolute path of `dafnydoc.jar` (in the `bin` directory).
8. Set `DAFNY_SERVER_PATH` to the absolute path of `dafny-server` in the `Binaries` subdirectory
   of the Dafny compiler.
9. Add the `bin` directory to your `PATH`.

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
