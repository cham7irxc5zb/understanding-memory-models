use super::{Loc, Op, Reg, Span};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Fence {
    Sc,
    Acq,
    Rel,
    AcqRel,
}

pub struct ExprVal {
    pub span: Span,
    pub guts: ExprGuts,
}

pub enum ExprGuts {
    Unit,
    Int {
        value: i64,
    },
    Reg {
        reg: Reg,
    },
    Loc {
        loc: Loc,
    },
    Monop {
        op: Op,
        arg: Expr,
    },
    Binop {
        op: Op,
        lhs: Expr,
        rhs: Expr,
    },
    Fence {
        kind: Fence,
    },
    Read {
        loc: Expr,
        is_acquire: bool,
    },
    Write {
        loc: Expr,
        value: Expr,
        is_release: bool,
    },
    Update {
        loc: Expr,
        reg: Expr,
        value: Expr,
        is_acquire: bool,
        is_release: bool,
    },
    Cond {
        cond: Expr,
        if_true: Expr,
        if_false: Expr,
    },
    LoopIf {
        body: Expr,
    },
    Block {
        stmts: Vec<Expr>,
        value: Expr,
    },
    Assign {
        reg: Expr,
        value: Expr,
    },
    Return {
        value: Expr,
    },
    Break {},
    Ignore {},
}

pub type Expr = Box<ExprVal>;

pub fn expr_unit(span: Span) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Unit,
    })
}

pub fn expr_int(span: Span, value: i64) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Int { value },
    })
}

pub fn expr_reg(span: Span, reg: Reg) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Reg { reg },
    })
}

pub fn expr_loc(span: Span, loc: Loc) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Loc { loc },
    })
}

pub fn expr_monop(op_span: Span, op: Op, arg: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[op_span, arg.span]),
        guts: ExprGuts::Monop { op, arg },
    })
}

pub fn expr_binop(op_span: Span, op: Op, lhs: Expr, rhs: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[lhs.span, op_span, rhs.span]),
        guts: ExprGuts::Binop { op, lhs, rhs },
    })
}

pub fn expr_fence(kw_span: Span, kind: Fence) -> Expr {
    Expr::new(ExprVal {
        span: kw_span,
        guts: ExprGuts::Fence { kind },
    })
}

pub fn expr_read(op_span: Span, is_acquire: bool, loc: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[op_span, loc.span]),
        guts: ExprGuts::Read { loc, is_acquire },
    })
}

pub fn expr_write(kw_span: Span, loc: Expr, is_release: bool, value: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[kw_span, loc.span, value.span]),
        guts: ExprGuts::Write {
            loc,
            is_release,
            value,
        },
    })
}

pub fn expr_update(
    kw_span: Span,
    is_acquire: bool,
    loc: Expr,
    reg: Expr,
    is_release: bool,
    value: Expr,
) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[kw_span, loc.span, reg.span, value.span]),
        guts: ExprGuts::Update {
            loc,
            is_acquire,
            reg,
            is_release,
            value,
        },
    })
}

pub fn expr_cond(if_span: Span, cond: Expr, if_true: Expr, if_false: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[if_span, cond.span, if_true.span, if_false.span]),
        guts: ExprGuts::Cond {
            cond,
            if_true,
            if_false,
        },
    })
}

pub fn expr_loop_if(span: Span, body: Expr) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::LoopIf { body },
    })
}

pub fn expr_assign(eq_span: Span, reg: Expr, value: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[reg.span, eq_span, value.span]),
        guts: ExprGuts::Assign { reg, value },
    })
}

pub fn expr_block(lbrace: usize, rbrace: usize, stmts: Vec<Expr>, value: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span(lbrace, rbrace),
        guts: ExprGuts::Block { stmts, value },
    })
}

pub fn expr_return(return_span: Span, value: Expr) -> Expr {
    Expr::new(ExprVal {
        span: Span::union(&[return_span, value.span]),
        guts: ExprGuts::Return { value },
    })
}

pub fn expr_break(span: Span) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Break {},
    })
}

pub fn expr_ignore(span: Span) -> Expr {
    Expr::new(ExprVal {
        span,
        guts: ExprGuts::Ignore {},
    })
}
