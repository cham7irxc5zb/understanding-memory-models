Building
---

This project is written in Rust. It can be built using `cargo`:

```
$ cargo build --release
```

The resulting binary is in `target/release/umm`.

Running
---

An example program can be checked by giving it to stdin:

```
target/release/umm < program.tlang
```

If the `-v` argument is given, a way to achieve each outcome is also printed.

Testing
---

The test suite can be run using `make`.

The two scripts `run-tree.sh` and `run-ast.py` start testing using the two
random test case generators.
