use super::{ast, lex};
use super::{Inst, Op, Proc, Program, Reg, Span, SyntaxError, Value};
use crate::index::*;
use crate::memory::Loc;
use crate::program::Fence;

struct Parser<'a> {
    lex: lex::Lexer<'a>,
    token: lex::Token,
    span: Span,
    locs: IMap<Loc, lex::Ident, Value>,
}

enum Name {
    Loc(Loc),
    Reg(Reg),
    Undefined,
}

impl<'a> Parser<'a> {
    #[inline]
    fn new(code: &'a [u8]) -> Self {
        let mut r = Self {
            lex: lex::Lexer::new(code),
            span: Span(0, 0),
            token: lex::Token::EOF,
            locs: IMap::new(),
        };
        r.skip();
        r
    }

    fn skip(&mut self) {
        let (tok, span) = self.lex.next();
        self.token = tok;
        self.span = span;
    }

    #[inline]
    fn error_at(&mut self, span: Span, msg: &'static str) -> Errors {
        self.lex.errors().push(SyntaxError { span, msg });
        Errors
    }

    #[inline]
    fn error(&mut self, msg: &'static str) -> Errors {
        self.error_at(self.span, msg)
    }

    fn expect(&mut self, tok: lex::Token, err: &'static str) -> Result<(), Errors> {
        if self.token == tok {
            Ok(())
        } else {
            Err(self.error(err))
        }
    }

    fn expect_ident(&mut self) -> Result<lex::Ident, Errors> {
        match self.token {
            lex::Token::Ident(id) => Ok(id),
            _ => Err(self.error("expected identifier")),
        }
    }

    fn parse_literal(&mut self) -> Result<Value, Errors> {
        match self.token {
            lex::Token::Int(n) => {
                self.skip();
                Ok(Value(n))
            }
            _ => Err(self.error("expected literal")),
        }
    }

    fn parse_top(&mut self) -> Result<Program, Errors> {
        let anon_base = isize::min_value() as usize;
        let mut procs = IMap::<usize, lex::Ident, Proc>::new();
        let mut anon_procs = Vec::<Proc>::new();
        let mut run = Vec::<(usize, _)>::new();
        loop {
            match self.token {
                lex::Token::EOF => {
                    let mut procs: Vec<_> =
                        procs.into_pairs().into_iter().map(|(_, f)| f).collect();
                    let anon0 = procs.len();
                    procs.extend(anon_procs);
                    for v in &mut run {
                        if v.0 >= anon_base {
                            v.0 = v.0 - anon_base + anon0;
                        }
                    }
                    return Ok(Program {
                        locs: self.locs.pairs().map(|_, (name, init_val)| {
                            let name = self.lex.ident(*name).to_owned();
                            (name, init_val.clone())
                        }),
                        procs,
                        run,
                    });
                }
                lex::Token::KwLoc => {
                    self.skip();
                    let name = self.expect_ident()?;
                    let name_span = self.span;
                    self.skip();
                    let init_val = if self.token == lex::Token::Assign {
                        self.skip();
                        self.parse_literal()?
                    } else {
                        Value(0)
                    };
                    self.expect(lex::Token::Semi, "expected `;'")?;
                    self.skip();

                    if self.locs.check_insert(name, init_val).is_err() {
                        return Err(self.error_at(name_span, "location already defined"));
                    }
                }
                lex::Token::KwProc => {
                    self.skip();
                    let name = self.expect_ident()?;
                    match procs.entry(&name) {
                        IMapEntry::Occupied(_) => {
                            return Err(self.error("proc already defined"));
                        }
                        IMapEntry::Vacant(e) => {
                            self.skip();
                            e.insert(name, self.parse_proc()?);
                        }
                    };
                }
                lex::Token::KwRun => {
                    self.skip();
                    if self.token == lex::Token::Lbrace {
                        run.push((anon_base + anon_procs.len(), Array::new_empty()));
                        anon_procs.push(self.parse_proc()?);
                    } else {
                        let name = self.expect_ident()?;
                        let (proc, nargs) = match procs.find(&name) {
                            Some(v) => (v.0, v.2.args),
                            None => return Err(self.error("proc not found")),
                        };
                        self.skip();
                        let mut args = Vec::new();
                        if self.token == lex::Token::Lparen {
                            self.skip();
                            self.parse_csl(lex::Token::Rparen, |p| {
                                args.push(p.parse_literal()?);
                                Ok(())
                            })?;
                        }
                        self.expect(lex::Token::Semi, "expected `;'")?;
                        let args = Array::new_vec(args);
                        if args.len() != nargs {
                            return Err(self.error("wrong number of arguments"));
                        }
                        self.skip();
                        run.push((proc, args));
                    }
                }
                _ => {
                    return Err(self.error("unexpected token"));
                }
            }
        }
    }

    fn parse_csl(
        &mut self,
        end: lex::Token,
        mut f: impl FnMut(&mut Self) -> Result<(), Errors>,
    ) -> Result<(), Errors> {
        loop {
            if self.token == end {
                self.skip();
                break;
            }
            f(self)?;
            let tok = self.token;
            self.skip();
            if tok == end {
                break;
            }
            if tok != lex::Token::Comma {
                return Err(self.error("expected end of list or comma"));
            }
        }
        Ok(())
    }

    fn parse_proc(&mut self) -> Result<Proc, Errors> {
        let mut regs = Length::ZERO;
        let mut args = IMap::<usize, lex::Ident, Reg>::new();

        if self.token == lex::Token::Lparen {
            self.skip();
            self.parse_csl(lex::Token::Rparen, |p| {
                if args.check_insert(p.expect_ident()?, regs.grow()).is_err() {
                    return Err(p.error("parameter already defined"));
                }
                p.skip();
                Ok(())
            })?;
        }

        let mut env = Env {
            regs_cur: regs,
            regs_max: regs,
            scope: args,
        };

        let args = regs;
        let e = self.parse_expr(&mut env)?;
        let regs = env.regs_max;
        let mut comp = Compiler::new(self.lex.errors());
        comp.gen_expr(&e, Break::forbid())?;
        comp.out.push(Inst::Return);
        let code = comp.out;

        Ok(Proc { args, regs, code })
    }

    fn parse_block(&mut self, env: &mut Env) -> Result<ast::Expr, Errors> {
        self.expect(lex::Token::Lbrace, "expected `{'")?;
        let begin = self.span.0;
        self.skip();

        let old_regs = env.regs_cur;
        let old_scope = env.scope.clone();

        let mut pre = Vec::new();
        let (end, last) = loop {
            match self.token {
                lex::Token::Rbrace => {
                    let end = self.span.1;
                    self.skip();
                    break (end, ast::expr_unit(Span(end, end)));
                }
                lex::Token::KwReg => {
                    if let Some(e) = self.parse_regdef(env)? {
                        pre.push(e);
                    }
                }
                _ => {
                    let (e, implicit_semi) = self.parse_stmt(env)?;
                    match self.token {
                        lex::Token::Rbrace => {
                            let end = self.span.1;
                            self.skip();
                            break (end, e);
                        }
                        lex::Token::Semi => {
                            self.skip();
                        }
                        _ if implicit_semi => {}
                        _ => {
                            env.regs_cur = old_regs;
                            env.scope = old_scope;
                            return Err(self.error("expected end of expression"));
                        }
                    }
                    pre.push(e);
                }
            }
        };

        env.regs_cur = old_regs;
        env.scope = old_scope;
        Ok(ast::expr_block(begin, end, pre, last))
    }

    fn parse_regdef(&mut self, env: &mut Env) -> Result<Option<ast::Expr>, Errors> {
        self.skip();

        let id = self.expect_ident()?;
        if self.locs.get(&id).is_some() {
            return Err(self.error("register name shadows location"));
        }
        let reg = env.regs_cur.grow();
        if env.regs_cur > env.regs_max {
            env.regs_max = env.regs_cur;
        }
        if env.scope.check_insert(id, reg).is_err() {
            return Err(self.error("register already defined"));
        }
        let reg_span = self.span;
        self.skip();

        let e = if self.token == lex::Token::Assign {
            let eq_span = self.span;
            self.skip();
            let e = self.parse_expr(env)?;
            Some(ast::expr_assign(eq_span, ast::expr_reg(reg_span, reg), e))
        } else {
            None
        };
        self.expect(lex::Token::Semi, "expected `;'")?;
        self.skip();
        Ok(e)
    }

    fn lookup_name(&mut self, env: &mut Env, id: lex::Ident) -> Name {
        if let Some(loc) = self.locs.get(&id) {
            return Name::Loc(loc);
        }

        if let Some((_, _, reg)) = env.scope.find(&id) {
            return Name::Reg(*reg);
        }

        Name::Undefined
    }

    fn parse_expr(&mut self, env: &mut Env) -> Result<ast::Expr, Errors> {
        Ok(self.parse_expr_binop(env, 0)?.0)
    }

    fn parse_stmt(&mut self, env: &mut Env) -> Result<(ast::Expr, bool), Errors> {
        let r = self.parse_expr_binop(env, 0)?;
        match self.token {
            lex::Token::Assign => {
                let span = self.span;
                self.skip();
                let (e, _) = self.parse_expr_binop(env, 0)?;
                Ok((ast::expr_assign(span, r.0, e), false))
            }
            _ => Ok(r),
        }
    }

    fn parse_expr_binop(&mut self, env: &mut Env, prec: u32) -> Result<(ast::Expr, bool), Errors> {
        if prec == 5 {
            return self.parse_expr_base(env);
        }
        let (mut r, mut implicit_semi) = self.parse_expr_binop(env, prec + 1)?;
        loop {
            let op = match self.token {
                lex::Token::Op(op) if op.prec() == Some(prec) => op,
                _ => break,
            };
            implicit_semi = false;
            let span = self.span;
            self.skip();
            let (e, _) = self.parse_expr_binop(env, prec + 1)?;
            r = ast::expr_binop(span, op, r, e);
        }
        Ok((r, implicit_semi))
    }

    fn parse_desig(&mut self, env: &mut Env) -> Result<ast::Expr, Errors> {
        match self.token {
            lex::Token::Ident(id) => {
                let r = match self.lookup_name(env, id) {
                    Name::Reg(reg) => ast::expr_reg(self.span, reg),
                    Name::Loc(loc) => ast::expr_loc(self.span, loc),
                    Name::Undefined => return Err(self.error("name not defined")),
                };
                self.skip();
                Ok(r)
            }
            _ => Err(self.error("unexpected token")),
        }
    }

    fn parse_expr_base(&mut self, env: &mut Env) -> Result<(ast::Expr, bool), Errors> {
        let mut monop_stack = Vec::new();
        while let lex::Token::Op(op) = self.token {
            if !op.is_monop() {
                break;
            }
            monop_stack.push((op, self.span));
            self.skip();
        }
        let mut implicit_semi = false;
        let mut e = match self.token {
            lex::Token::Lbrace => {
                implicit_semi = true;
                self.parse_block(env)?
            }
            lex::Token::Int(v) => {
                let r = ast::expr_int(self.span, v);
                self.skip();
                r
            }
            lex::Token::Lparen => {
                let left = self.span.0;
                self.skip();
                let mut r = self.parse_expr(env)?;
                self.expect(lex::Token::Rparen, "expected `)'")?;
                let right = self.span.1;
                self.skip();
                r.span = Span(left, right);
                r
            }
            lex::Token::KwIf => {
                implicit_semi = true;
                let mut ecases = Vec::new();
                let mut e = loop {
                    let if_span = self.span;
                    self.skip();
                    {
                        let econd = self.parse_expr(env)?;
                        let elhs = self.parse_block(env)?;
                        ecases.push((if_span, econd, elhs));
                    }
                    if self.token != lex::Token::KwElse {
                        break ast::expr_unit(if_span);
                    }
                    self.skip();
                    if self.token != lex::Token::KwIf {
                        break self.parse_block(env)?;
                    }
                };
                while let Some((if_span, econd, elhs)) = ecases.pop() {
                    e = ast::expr_cond(if_span, econd, elhs, e);
                }
                e
            }
            lex::Token::KwLoop => {
                let begin = self.span.0;
                self.skip();
                self.expect(lex::Token::KwIf, "expected `if'")?;
                self.skip();
                let body = self.parse_expr(env)?;
                ast::expr_loop_if(Span(begin, body.span.1), body)
            }
            lex::Token::KwSc => {
                let span = self.span;
                self.skip();
                ast::expr_fence(span, ast::Fence::Sc)
            }
            lex::Token::KwAcq => {
                let mut span = self.span;
                self.skip();
                if self.token == lex::Token::KwRel {
                    span.1 = self.span.1;
                    self.skip();
                    ast::expr_fence(span, ast::Fence::AcqRel)
                } else {
                    ast::expr_fence(span, ast::Fence::Acq)
                }
            }
            lex::Token::KwRel => {
                let span = self.span;
                self.skip();
                ast::expr_fence(span, ast::Fence::Rel)
            }
            lex::Token::KwRead => {
                let mut span = self.span;
                self.skip();
                let mut is_acquire = false;
                if self.token == lex::Token::KwAcq {
                    is_acquire = true;
                    span.1 = self.span.1;
                    self.skip();
                }
                let loc = self.parse_desig(env)?;
                ast::expr_read(span, is_acquire, loc)
            }
            lex::Token::KwWrite => {
                let mut span = self.span;
                self.skip();
                let mut is_release = false;
                if self.token == lex::Token::KwRel {
                    is_release = true;
                    span.1 = self.span.1;
                    self.skip();
                }
                let loc = self.parse_desig(env)?;
                let value = self.parse_expr(env)?;
                ast::expr_write(span, loc, is_release, value)
            }
            lex::Token::KwUpdate => {
                let mut span = self.span;
                self.skip();
                let mut is_acquire = false;
                if self.token == lex::Token::KwAcq {
                    is_acquire = true;
                    span.1 = self.span.1;
                    self.skip();
                }
                let mut is_release = false;
                if self.token == lex::Token::KwRel {
                    is_release = true;
                    span.1 = self.span.1;
                    self.skip();
                }
                let loc = self.parse_desig(env)?;
                let reg = self.parse_desig(env)?;
                let value = self.parse_expr(env)?;
                ast::expr_update(span, is_acquire, loc, reg, is_release, value)
            }
            lex::Token::KwReturn => {
                let span = self.span;
                self.skip();
                let e = self.parse_expr(env)?;
                ast::expr_return(span, e)
            }
            lex::Token::KwBreak => {
                let span = self.span;
                self.skip();
                ast::expr_break(span)
            }
            lex::Token::KwIgnore => {
                let span = self.span;
                self.skip();
                ast::expr_ignore(span)
            }
            _ => self.parse_desig(env)?,
        };
        while let Some((op, span)) = monop_stack.pop() {
            implicit_semi = false;
            e = ast::expr_monop(span, op, e);
        }
        Ok((e, implicit_semi))
    }
}

pub fn parse(code: &[u8]) -> Result<Program, Vec<SyntaxError>> {
    let mut p = Parser::new(code);
    let r = p.parse_top();
    let errors = p.lex.into_errors();
    match r {
        Ok(p) => {
            assert!(errors.is_empty());
            Ok(p)
        }
        Err(Errors) => {
            assert!(!errors.is_empty());
            Err(errors)
        }
    }
}

struct Env {
    regs_max: Length<Reg>,
    regs_cur: Length<Reg>,
    scope: IMap<usize, lex::Ident, Reg>,
}

struct Compiler<'a> {
    out: Vec<Inst>,
    errors: &'a mut Vec<SyntaxError>,
}

struct Errors;

struct JumpPatch {
    insts: Vec<usize>,
}

impl JumpPatch {
    fn new() -> Self {
        Self { insts: Vec::new() }
    }

    fn is_empty(&self) -> bool {
        self.insts.is_empty()
    }
}

struct Break<'a> {
    pop: usize,
    patch: Option<&'a mut JumpPatch>,
}

impl<'a> Break<'a> {
    fn forbid() -> Self {
        Self {
            pop: 0,
            patch: None,
        }
    }

    fn new(patch: &'a mut JumpPatch) -> Self {
        Self {
            pop: 0,
            patch: Some(patch),
        }
    }

    fn borrow<'b>(&'b mut self) -> Break<'b> {
        Break {
            pop: self.pop,
            patch: match &mut self.patch {
                Some(a) => Some(a),
                None => None,
            },
        }
    }

    fn with_pop(mut self, n: usize) -> Self {
        self.pop += n;
        self
    }
}

impl<'a> Compiler<'a> {
    fn new(errors: &'a mut Vec<SyntaxError>) -> Self {
        Self {
            out: Vec::new(),
            errors,
        }
    }

    fn gen_expr(&mut self, e: &ast::ExprVal, mut brk: Break) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Int { value } => {
                self.out.push(Inst::ConstInt(*value));
                Ok(())
            }
            ExprGuts::Reg { reg } => {
                self.out.push(Inst::Get(*reg));
                Ok(())
            }
            ExprGuts::Monop { op, arg, .. } => {
                self.gen_expr(arg, brk)?;
                self.out.push(Inst::Monop(*op));
                Ok(())
            }
            ExprGuts::Binop { op, lhs, rhs, .. } => {
                match op {
                    Op::LogicalAnd => {
                        let mut patch_true = JumpPatch::new();
                        let lhs = self.gen_cond(lhs, brk.borrow(), true, &mut patch_true);
                        self.out.push(Inst::ConstInt(0));
                        let patch_end = self.out.len();
                        self.out.push(Inst::Jump(!0));
                        let tgt_true = self.out.len();
                        let rhs = self.gen_expr(rhs, brk);
                        let tgt_end = self.out.len();
                        lhs?;
                        rhs?;
                        self.patch_jump(patch_end, tgt_end);
                        self.patch_jumps(patch_true, tgt_true);
                    }
                    Op::LogicalOr => {
                        let mut patch_false = JumpPatch::new();
                        let lhs = self.gen_cond(lhs, brk.borrow(), false, &mut patch_false);
                        self.out.push(Inst::ConstInt(0));
                        let patch_end = self.out.len();
                        self.out.push(Inst::Jump(!0));
                        let tgt_false = self.out.len();
                        let rhs = self.gen_expr(rhs, brk);
                        let tgt_end = self.out.len();
                        lhs?;
                        rhs?;
                        self.patch_jump(patch_end, tgt_end);
                        self.patch_jumps(patch_false, tgt_false);
                    }
                    _ => {
                        let lhs = self.gen_expr(lhs, brk.borrow());
                        let rhs = self.gen_expr(rhs, brk.with_pop(1));
                        lhs?;
                        rhs?;
                        self.out.push(Inst::Binop(*op));
                    }
                }
                Ok(())
            }
            ExprGuts::Read { loc, is_acquire } => {
                let is_acquire = *is_acquire;
                self.gen_loc_read(loc, is_acquire)?;
                Ok(())
            }
            ExprGuts::Cond {
                cond,
                if_true,
                if_false,
            } => {
                let mut patch_false = JumpPatch::new();
                let k0 = self.gen_cond(cond, brk.borrow(), false, &mut patch_false);
                let k1 = self.gen_expr(if_true, brk.borrow());
                let patch_end = self.out.len();
                self.out.push(Inst::Jump(!0));
                let tgt_false = self.out.len();
                let k2 = self.gen_expr(if_false, brk);
                k0?;
                k1?;
                k2?;
                self.patch_jumps(patch_false, tgt_false);
                self.patch_jump(patch_end, self.out.len());
                Ok(())
            }
            ExprGuts::Block { stmts, value, .. } => {
                let mut err = false;
                for stmt in stmts {
                    if self.gen_stmt(stmt, brk.borrow()).is_err() {
                        err = true;
                    }
                }
                if self.gen_expr(value, brk).is_err() {
                    err = true;
                }
                if err {
                    return Err(Errors);
                }
                Ok(())
            }
            ExprGuts::Return { value } => {
                self.gen_expr(value, brk)?;
                self.out.push(Inst::Return);
                Ok(())
            }
            ExprGuts::Break {} => self.gen_break(&e.span, brk),
            ExprGuts::Ignore {} => self.gen_ignore(&e.span),
            _ => Err(self.error(e.span, "expected expression")),
        }
    }

    fn gen_stmt(&mut self, e: &ast::ExprVal, mut brk: Break) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Unit => Ok(()),
            ExprGuts::Fence { kind } => {
                match kind {
                    ast::Fence::Sc => {
                        self.out.push(Inst::Fence(Fence::Sc));
                    }
                    ast::Fence::Acq => {
                        self.out.push(Inst::Fence(Fence::Acq));
                    }
                    ast::Fence::Rel => {
                        self.out.push(Inst::Fence(Fence::Rel));
                    }
                    ast::Fence::AcqRel => {
                        self.out.push(Inst::Fence(Fence::Rel));
                    }
                }
                Ok(())
            }
            ExprGuts::Write {
                loc,
                is_release,
                value,
            } => {
                let is_release = *is_release;
                let rhs = self.gen_expr(value, brk);
                let lhs = self.gen_loc_write(loc, is_release);
                lhs?;
                rhs?;
                Ok(())
            }
            ExprGuts::Update {
                loc,
                reg,
                value,
                is_acquire,
                is_release,
            } => {
                let is_acquire = *is_acquire;
                let is_release = *is_release;
                let op1 = self.gen_loc_update(loc, is_acquire);
                let op2 = self.gen_assign(reg);
                let mut ubrk = JumpPatch::new();
                let op3 = self.gen_expr(value, Break::new(&mut ubrk));
                op1?;
                op2?;
                op3?;
                self.out.push(Inst::CommitUpdate(is_release));

                if !ubrk.is_empty() {
                    let patch_end = self.out.len();
                    self.out.push(Inst::Jump(!0));
                    self.patch_jumps(ubrk, self.out.len());
                    self.out.push(Inst::CancelUpdate);
                    self.patch_jump(patch_end, self.out.len());
                }
                Ok(())
            }
            ExprGuts::Assign { reg, value } => {
                let rhs = self.gen_expr(value, brk);
                let lhs = self.gen_assign(reg);
                lhs?;
                rhs?;
                Ok(())
            }
            ExprGuts::Cond {
                cond,
                if_true,
                if_false,
            } => {
                let mut patch_false = JumpPatch::new();
                let k0 = self.gen_cond(cond, brk.borrow(), false, &mut patch_false);
                let k1 = self.gen_stmt(if_true, brk.borrow());
                let patch_end = self.out.len();
                self.out.push(Inst::Jump(!0));
                let tgt_false = self.out.len();
                let k2 = self.gen_stmt(if_false, brk);
                k0?;
                k1?;
                k2?;
                self.patch_jumps(patch_false, tgt_false);
                self.patch_jump(patch_end, self.out.len());
                Ok(())
            }
            ExprGuts::LoopIf { body } => {
                let repeat = self.out.len();
                let mut patch_true = JumpPatch::new();
                let k0 = self.gen_cond(body, brk, true, &mut patch_true);
                k0?;
                self.patch_jumps(patch_true, repeat);
                Ok(())
            }
            ExprGuts::Block { stmts, value, .. } => {
                let mut err = false;
                for stmt in stmts {
                    if self.gen_stmt(stmt, brk.borrow()).is_err() {
                        err = true;
                    }
                }
                if self.gen_stmt(value, brk).is_err() {
                    err = true;
                }
                if err {
                    return Err(Errors);
                }
                Ok(())
            }
            ExprGuts::Return { value } => {
                self.gen_expr(value, brk)?;
                self.out.push(Inst::Return);
                Ok(())
            }
            ExprGuts::Break {} => self.gen_break(&e.span, brk),
            ExprGuts::Ignore {} => self.gen_ignore(&e.span),
            _ => Err(self.error(e.span, "expected statement")),
        }
    }

    fn gen_assign(&mut self, e: &ast::ExprVal) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Reg { reg } => {
                self.out.push(Inst::Set(*reg));
                Ok(())
            }
            _ => {
                self.error(e.span, "expected register");
                Err(Errors)
            }
        }
    }

    fn gen_loc_read(&mut self, e: &ast::ExprVal, is_acquire: bool) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Loc { loc } => {
                self.out.push(Inst::Read(*loc, is_acquire));
                Ok(())
            }
            _ => {
                self.error(e.span, "expected location");
                Err(Errors)
            }
        }
    }

    fn gen_loc_update(&mut self, e: &ast::ExprVal, is_acquire: bool) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Loc { loc } => {
                self.out.push(Inst::Update(*loc, is_acquire));
                Ok(())
            }
            _ => {
                self.error(e.span, "expected location");
                Err(Errors)
            }
        }
    }

    fn gen_loc_write(&mut self, e: &ast::ExprVal, is_release: bool) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Loc { loc } => {
                self.out.push(Inst::Write(*loc, is_release));
                Ok(())
            }
            _ => {
                self.error(e.span, "expected location");
                Err(Errors)
            }
        }
    }

    fn gen_cond(
        &mut self,
        e: &ast::ExprVal,
        mut brk: Break,
        gen_if: bool,
        patch: &mut JumpPatch,
    ) -> Result<(), Errors> {
        use ast::ExprGuts;
        match &e.guts {
            ExprGuts::Monop {
                op: Op::LogicalNot,
                arg,
                ..
            } => self.gen_cond(arg, brk, !gen_if, patch),
            ExprGuts::Binop { op, lhs, rhs }
                if gen_if && *op == Op::LogicalOr || !gen_if && *op == Op::LogicalAnd =>
            {
                let lhs = self.gen_cond(lhs, brk.borrow(), gen_if, patch);
                let rhs = self.gen_cond(rhs, brk, gen_if, patch);
                lhs?;
                rhs?;
                Ok(())
            }
            ExprGuts::Binop { op, lhs, rhs }
                if !gen_if && *op == Op::LogicalOr || gen_if && *op == Op::LogicalAnd =>
            {
                let mut patch_fastfail = JumpPatch::new();
                let lhs = self.gen_cond(lhs, brk.borrow(), !gen_if, &mut patch_fastfail);
                let rhs = self.gen_cond(rhs, brk, gen_if, patch);
                lhs?;
                rhs?;
                let tgt_fastfail = self.out.len();
                self.patch_jumps(patch_fastfail, tgt_fastfail);
                Ok(())
            }
            _ => {
                self.gen_expr(e, brk)?;
                if gen_if {
                    self.out.push(Inst::Monop(Op::LogicalNot));
                }
                patch.insts.push(self.out.len());
                self.out.push(Inst::JumpIfZero(!0));
                Ok(())
            }
        }
    }

    fn gen_break(&mut self, span: &Span, brk: Break) -> Result<(), Errors> {
        match brk.patch {
            Some(patch) => {
                for _ in 0..brk.pop {
                    self.out.push(Inst::Pop);
                }
                patch.insts.push(self.out.len());
                self.out.push(Inst::Jump(!0));
                Ok(())
            }
            None => Err(self.error(*span, "`break' not allowed here")),
        }
    }

    fn gen_ignore(&mut self, _span: &Span) -> Result<(), Errors> {
        self.out.push(Inst::Ignore);
        Ok(())
    }

    fn patch_jump(&mut self, at: usize, tgt: usize) {
        match &mut self.out[at] {
            Inst::Jump(to) | Inst::JumpIfZero(to) => {
                assert_eq!(*to, !0);
                *to = tgt;
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn patch_jumps(&mut self, at: JumpPatch, tgt: usize) {
        for at in at.insts {
            self.patch_jump(at, tgt);
        }
    }

    fn error(&mut self, span: Span, msg: &'static str) -> Errors {
        self.errors.push(SyntaxError { span, msg });
        Errors
    }
}
