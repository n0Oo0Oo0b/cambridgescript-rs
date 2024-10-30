# CambridgeScript 🦀

Blazing fast interpreter for Cambridge IGCSE pseudocode

The specification for this language is a subset of the [AS & A Level pseudocode](https://www.cambridgeinternational.org/Images/697401-2026-pseudocode-guide-for-teachers.pdf).
The (mostly accurate) grammer can be found in the [old Python repo](https://github.com/n0Oo0Oo0b/pseudo-interpreter/tree/main/cambridgeScript/parser/grammar.txt).

Feel free to open an issue/PR if you have any suggestions/improvements!

## Install

### Download

Go to the [releases](https://github.com/n0Oo0Oo0b/cambridgescript-rs/releases) tab and download the correct version under assets.
Currently only x86 Windows, x86 Linux and Apple silicon Mac are available for download until I figure out how to use GitHub actions.

### Self-compile

Requires `git` and `cargo` with a nightly toolchain

```sh
$ git clone https://github.com/n0Oo0Oo0b/cambridgescript-rs.git
$ cd cambridgescript-rs
$ cargo run --release
```

### Nix

Coming soon

## Usage

Running the executable without ay arguments opens a bare-bones REPL (Ctrl+C to exit):

```sh
$ ./cambridgescript
CambridgeScript REPL
 1 │ DECLARE x : INTEGER
 2 │ x <- 42
 3 │ OUTPUT 42
 4 │
42
```

Use `cambridgescript [file]` to execute a file as a script:

```sh
$ cat factorial.txt
DECLARE num : INTEGER
DECLARE result : INTEGER

num <- 6
result <- 1

WHILE num > 1 DO
    result <- result * num
    num <- num - 1
ENDWHILE

OUTPUT "6! = ", result

$ ./cambridgescript factorial.txt
6! = 720
```

## Language features

Statements
- [ ] `INPUT`
- [x] `OUTPUT`
- [x] `a <- b`
- [x] `IF` / `IF ELSE`
- [ ] `CASE OF`
- [ ] `FOR`
- [x] `WHILE`
- [x] `REPEAT ... UNTIL`
- [x] `DECLARE` (Only for primitive types)
- [ ] `CONSTANT`
- [ ] `PROCEDURE`
- [ ] `FUNCTION`
- [ ] `FILE(OPEN|READ|WRITE|CLOSE)`
- [ ] `CALLPROCEDURE`

## Roadmap

- Implement remaining statements
- Arrays
- Nicer errors
- LSP integration (+ VSCode extension)
- Test coverage
