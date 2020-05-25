use std::{env, io};

pub mod util;

pub mod htab;

// #[path = "nconcmap.rs"]
pub mod concmap;

#[macro_use]
pub mod index;

pub mod memory;

pub mod program;

pub mod tlang;

pub mod thread;

pub mod system;

pub mod consistency;

pub mod checkpoint;

pub mod promise;

pub mod explorer;

fn show_errors(code: &[u8], errors: Vec<tlang::SyntaxError>) {
    let mut lines = Vec::new();
    for (i, v) in code.iter().cloned().enumerate() {
        if v == b'\n' {
            lines.push(i);
        }
    }
    lines.push(code.len());

    for err in errors {
        let l = match lines.binary_search(&err.span.0) {
            Ok(l) => l,
            Err(l) => l,
        };
        let lstart = if l == 0 { 0 } else { lines[l - 1] + 1 };
        let lend = if l == lines.len() { lstart } else { lines[l] };

        use std::io::Write;
        let mut buf = Vec::new();
        write!(&mut buf, "{}: {}\n\t", l + 1, err.msg).unwrap();
        for c in code[lstart..lend].iter().cloned() {
            if c == b'\t' {
                buf.extend_from_slice(b"    ");
            } else {
                buf.push(c);
            }
        }
        buf.extend_from_slice(b"\n\t");
        let mut n = 0;
        for c in code[lstart..err.span.0].iter().cloned() {
            n += if c == b'\t' { 4 } else { 1 };
        }
        buf.resize(buf.len() + n, b' ');
        n = 0;
        for c in code[err.span.0..(err.span.1 + 1).min(lend)].iter().cloned() {
            n += if c == b'\t' { 4 } else { 1 };
        }
        buf.resize(buf.len() + n, b'^');
        buf.extend_from_slice(b"\n\n");
        io::stderr().write_all(&buf).unwrap();
    }
}

fn show_output<W: io::Write>(
    out: &mut W,
    dec: &program::ValueDecoder<tlang::Value>,
    x: &explorer::Explorer,
    vals: &[(index::Array<system::Tid, memory::Val>, usize)],
    verbose: bool,
) -> io::Result<()> {
    for (v, prev) in vals {
        let v = &**v;
        let prev = *prev;
        let steps = x.explain(prev);

        let last = v.len().as_usize() - 1;
        for (i, val) in v.iter().cloned().enumerate() {
            let val = &dec.ret[val];
            write!(out, "{}", val)?;
            out.write_all(if i == last { b"\n" } else { b" " })?;
        }

        if verbose {
            for step in steps {
                write!(out, "    [thread {}] ", step.thread.0 + 1)?;
                match step.action {
                    system::ExplainAction::Promise(p) => writeln!(
                        out,
                        "{}promise {} {} := {} @ {}",
                        match p.kind {
                            memory::PromiseKind::New => "",
                            memory::PromiseKind::Split => "split ",
                        },
                        if p.is_update { "update" } else { "write" },
                        dec.loc_names[p.loc],
                        dec.loc[p.loc][p.val],
                        p.t,
                    ),
                    system::ExplainAction::Sc => writeln!(out, "sc fence"),
                    system::ExplainAction::Rel => writeln!(out, "release fence"),
                    system::ExplainAction::RelWrite(w) => writeln!(
                        out,
                        "release {} {} := {} @ {}",
                        if w.is_update { "update" } else { "write" },
                        dec.loc_names[w.loc],
                        dec.loc[w.loc][w.val],
                        w.t,
                    ),
                }?
            }
        }
    }
    Ok(())
}

fn main() {
    let mut verbose = false;
    let mut nthr = 1;
    let mut opt = program::OptFlags::default();

    let mut args = env::args_os();
    args.next().unwrap();
    for arg in args {
        if let Some(arg_str) = arg.to_str() {
            if arg_str == "-v" {
                verbose = true;
                continue;
            }
            if arg_str == "-noopt-compress" {
                opt.compress = false;
                continue;
            }
            if arg_str == "-noopt-equiv" {
                opt.equiv = false;
                continue;
            }
            if arg_str == "-noopt-local" {
                opt.local = false;
                continue;
            }
            if arg_str == "-noopt-promise" {
                opt.promise = false;
                continue;
            }
            if arg_str == "-noopt-cc" {
                opt.cc = false;
                continue;
            }
            if arg_str.starts_with("-t") {
                nthr = arg_str[2..].parse::<usize>().unwrap();
                continue;
            }
        }
        panic!("Unknown argument: {:?}", arg);
    }

    let code = {
        let mut r = Vec::new();
        io::Read::read_to_end(&mut io::stdin().lock(), &mut r).unwrap();
        r
    };

    let prog = match tlang::parse(&code) {
        Ok(p) => p,
        Err(errors) => {
            show_errors(&code, errors);
            return;
        }
    };

    if verbose {
        eprintln!("Parsed program");
    }

    let (prog, dec, threads) = prog.compile(opt);
    if verbose {
        eprintln!("    Thread-local states: {}", prog.n_states());
        eprintln!("       inner-equivalent: {}", prog.n_inner_equiv());
    }

    use std::sync::Arc;
    let x = Arc::new(explorer::Explorer::new(prog, nthr));
    x.new_state(&threads);

    use std::thread;
    let mut threads = Vec::new();
    for _ in 0..nthr {
        let xr = x.clone();
        threads.push(thread::spawn(move || {
            xr.explore();
        }));
    }

    for thr in threads {
        thr.join().unwrap();
    }

    // x.explore();

    x.report();

    let vals = x.out();
    let out = io::stdout();
    let mut out = io::BufWriter::new(out.lock());
    show_output(&mut out, &dec, &x, &vals, verbose).unwrap();
}
