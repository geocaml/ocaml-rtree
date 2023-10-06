# Weclome

Thank you for taking interest in this repository and considering contributing! If you need to get setup with OCaml please follow the excellent instructions at https://ocaml.org/docs.

## Installing Dependencies

After cloning the repository and `cd`-ing into it, you should be able to run

```
opam install . --deps-only --with-test
```

which will install the dependencies of the project.

## Building and Running the Tests

We use `dune` as the build tool for this repository. To build the project you can run `dune build` and to run the tests use `dune runtest`.
