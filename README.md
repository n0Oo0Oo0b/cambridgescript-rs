# cambridgescript-rs

Interpreter for Cambridge IGCSE pseudocode

## Usage

Running the executable without ay arguments opens a bare-bones REPL:

```sh
$ cambridgescript
> DECLARE x: INTEGER
> x <- 2
> OUTPUT "x is ", x
x is 2
>
```

Use `cambridgescript [file]` to run a script from a file:

```sh
$ cat script.txt
DECLARE x: INTEGER
x <- 2
OUTPUT "x is ", x

$ cambridgescript script.txt
x is 2
```

## Things implemented so far

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
- Multi-line input in REPL
- LSP integration (+ VSCode extension)
