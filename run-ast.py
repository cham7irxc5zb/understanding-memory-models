#!/usr/bin/env python3
from random import Random, getrandbits as random_bits, choice as random_choice, randrange as random_int, shuffle as random_shuffle
import math, itertools, sys, io, subprocess

def random_poisson(avg, min=0, max=None, _math_exp=math.exp):
    while True:
        scale = _math_exp(avg)

        v = 1.0
        start = 0.0
        i = 0
        while i < min:
            scale -= v
            i += 1
            v *= avg / i

        scale *= random_bits(54) / 2**54
        while max is None or i <= max:
            if scale <= v:
                return i
            scale -= v
            i += 1
            v *= avg / i

class Expr:
    __slots__ = ()

    def render(self, ctx, /):
        raise NotImplementedError()

class ConstExpr(Expr):
    __slots__ = ('val',)

    def __init__(self, val):
        self.val = val

    def render(self, ctx, /):
        ctx.write(f'{self.val}')

class Var(Expr):
    __slots__ = ('id',)
    COUNTER = 0

    def __init__(self, /):
        self.id = id = Var.COUNTER
        Var.COUNTER = id + 1

    def render(self, ctx, /):
        if ctx.vars_by_id.setdefault(self.id, self) is not self:
            raise RuntimeError(f'duplicate variable: `r{self.id}\'')
        try:
            v = ctx.aliases[self]
        except KeyError:
            pass
        else:
            return v.render(ctx)
        if self not in ctx.visible_vars:
            raise RuntimeError(f'variable out of scope: `r{self.id}\'')
        ctx.write(f'r{self.id}')

class VarW:
    __slots__ = ('v',)

    def __init__(self, v, /):
        self.v = v

    def render(self, ctx, /):
        v = self.v
        if ctx.vars_by_id.setdefault(v.id, v) is not v:
            raise RuntimeError(f'duplicate variable: `r{v.id}\'')
        try:
            v2 = ctx.aliases[v]
        except KeyError:
            pass
        else:
            raise RuntimeError(f'writing to alias: `r{v.id}\' -> `r{v2.id}\'')
        if self in ctx.visible_vars:
            raise RuntimeError(f'variable already in scope: `r{v.id}\'')
        ctx.write(f'r{v.id}')

class Block:
    __slots__ = ('body', 'vars', 'parent', 'phi_edges', 'returns', 'aliases')

    def __init__(self, /, parent):
        assert parent is None or isinstance(parent, Stmt)
        self.parent = parent
        self.body = []
        self.phi_edges = []
        self.returns = None
        self.aliases = {}

    def render(self, ctx, /):
        ctx.aliases.update(self.aliases)
        vis = {*ctx.visible_vars}
        assert vis == frozenset(self.visible_vars(0))
        ctx.write('{')
        ctx.stmt_end()
        ctx.indent()
        for s in self.body:
            s.render(ctx)
        assert ctx.visible_vars == frozenset(self.visible_vars())
        for var,e in self.phi_edges:
            ctx.stmt_begin()
            ctx.write(VarW(var), ' = ', e, ';')
            ctx.stmt_end()
        if self.returns is not None:
            ctx.stmt_begin()
            ctx.write(self.returns)
            ctx.stmt_end()
        ctx.dedent()
        ctx.stmt_begin()
        ctx.write('}')
        ctx.visible_vars = vis

    def add(self, st, /, *args, **kw):
        s = Stmt.__new__(st)
        s.scope = self
        self.body.append(s)
        st.__init__(s, *args, **kw)
        return s

    def insert(self, i, st, /, *args, **kw):
        s = Stmt.__new__(st)
        s.scope = self
        self.body.insert(i, s)
        st.__init__(s, *args, **kw)
        return s

    def visible_vars(self, upto=None, /):
        if upto is None:
            upto = len(self.body)
        elif type(upto) is int:
            pass
        else:
            upto = self.body.index(upto)
        v = []
        while True:
            if upto:
                for st in self.body[upto-1::-1]:
                    v.extend(st.vars)
            par = self.parent
            if par is None:
                break
            self = par.scope
            upto = self.body.index(par)
        assert len(v) == len(frozenset(v))
        return v

    def phi_edge(self, var, e):
        self.phi_edges.append((var, e))

    def clone(self, parent2, /):
        assert (self.parent is None) == (parent2 is None)
        b2 = Block(parent2)
        for s in self.body:
            st = type(s)
            s2 = Stmt.__new__(st)
            s2.scope = b2
            i = len(b2.body)
            b2.body.append(s2)
            st.clone_from(s2, s)
            assert s.vars == s2.vars
        b2.phi_edges.extend(self.phi_edges)
        b2.aliases.update(self.aliases)
        b2.returns = self.returns
        return b2

    def add_alias(self, v1, v2):
        self.aliases[v1] = v2

class Stmt:
    __slots__ = ('scope',)

    @property
    def vars(self, /):
        return ()

    def render(self, ctx, /):
        self.render_pre(ctx)

        ctx.stmt_begin()
        r = self.render_inner(ctx)
        ctx.stmt_end()

        vv = ctx.visible_vars
        vs = frozenset(self.vars)
        if r:
            assert vs <= vv
        else:
            assert not vv & vs
            vv.update(vs)

    def clone_from(self, s, /):
        raise NotImplementedError

    def render_pre(self, ctx, /):
        pass

    def render_inner(self, ctx, /):
        raise NotImplementedError

class ScStmt(Stmt):
    __slots__ = ()

    def clone_from(self, s, /):
        pass

    def render_inner(self, ctx, /):
        ctx.write('sc;')

class AcqStmt(Stmt):
    __slots__ = ()

    def clone_from(self, s, /):
        pass

    def render_inner(self, ctx, /):
        ctx.write('acq;')

class RelStmt(Stmt):
    __slots__ = ()

    def clone_from(self, s, /):
        pass

    def render_inner(self, ctx, /):
        ctx.write('rel;')

class ReadStmt(Stmt):
    __slots__ = ('var', 'loc', 'is_acq')

    def __init__(self, /, loc, is_acq):
        self.var = Var()
        self.loc = loc
        self.is_acq = is_acq

    def clone_from(self, s, /):
        self.var = s.var
        self.loc = s.loc
        self.is_acq = s.is_acq

    @property
    def vars(self, /):
        return (self.var,)

    def render_inner(self, ctx, /):
        ctx.write('reg ', VarW(self.var), ' = read ', 'acq ' if self.is_acq else None, self.loc, ';')

class WriteStmt(Stmt):
    __slots__ = ('loc', 'val', 'is_rel')

    def __init__(self, /, loc, val, is_rel):
        self.loc = loc
        self.val = val
        self.is_rel = is_rel

    def clone_from(self, s, /):
        self.loc = s.loc
        self.val = s.val
        self.is_rel = s.is_rel

    def render_inner(self, ctx, /):
        ctx.write('write ', 'rel ' if self.is_rel else None, self.loc, ' ', self.val, ';')

class UpdateStmt(Stmt):
    __slots__ = ('loc', 'var', 'val', 'is_acq', 'is_rel')

    def __init__(self, /, loc, val, is_acq, is_rel):
        var = Var()
        val = val(var, self.scope.visible_vars(self))
        self.loc = loc
        self.var = var
        self.val = val
        self.is_acq = is_acq
        self.is_rel = is_rel

    def clone_from(self, s, /):
        self.loc = s.loc
        self.var = s.var
        self.val = s.val
        self.is_acq = s.is_acq
        self.is_rel = s.is_rel

    @property
    def vars(self, /):
        return (self.var,)

    def render_pre(self, ctx, /):
        ctx.stmt_begin()
        ctx.write('reg ', VarW(self.var), ';')
        ctx.stmt_end()

    def render_inner(self, ctx, /):
        ctx.visible_vars.add(self.var)
        ctx.write('update ', 'acq ' if self.is_acq else None, 'rel ' if self.is_rel else None, self.loc, ' ', self.var, ' ', self.val, ';')
        return True

class IfStmt(Stmt):
    __slots__ = ('conds', 'phi')

    def __init__(self, /, n_phi, cases, gen_case):
        self.phi = phi = (*(Var() for i in range(n_phi)),)

        conds = []
        for i,cond in itertools.chain(enumerate(cases), [(None, None)]):
            blk = Block(self)
            assign = gen_case(blk, i)
            assert len(assign) == len(self.phi)
            for var,e in zip(self.phi, assign):
                assert isinstance(e, Expr)
                blk.phi_edge(var, e)
            conds.append((cond, blk))
        self.conds = conds

    @property
    def vars(self):
        return self.phi

    def clone_from(self, s, /):
        self.conds = [(cond, blk.clone(self)) for (cond,blk) in s.conds]
        self.phi = s.phi

    def with_cond(self, cond, /):
        def decorate(f, /):
            blk = Block(self)
            assign = (*f(blk),)
            return blk

    def render_pre(self, ctx, /):
        for v in self.vars:
            ctx.stmt_begin()
            ctx.write('reg ', VarW(v), ';')
            ctx.stmt_end()

    def render_inner(self, ctx, /):
        for cond,blk in self.conds[:-1]:
            ctx.write('if ', cond, ' ', blk, ' else ')
        ctx.write(self.conds[-1][1])

class MixExpr(Expr):
    __slots__ = ('vars', 'coefs', 'mod')

    def __init__(self, /, vars, seed, mod):
        self.vars = (*vars,)
        n = len(vars)
        rng = Random(seed)
        self.coefs = [rng.randrange(10007) for i in range((n+1) * (n+2) // 2)]
        self.mod = mod

    def render(self, ctx, /):
        coefs = iter(self.coefs)
        ctx.write('((', f'{next(coefs)}')
        for i,v1 in enumerate(self.vars):
            ctx.write('+', f'{next(coefs)}', '*', v1)
            for v2 in self.vars[:i+1]:
                ctx.write('+', f'{next(coefs)}', '*', v1, '*', v2)
        ctx.write(') % 10007', None if self.mod is None else f' % {self.mod}', ')')

class EqExpr(Expr):
    __slots__ = ('e1', 'e2')

    def __init__(self, /, e1, e2):
        self.e1 = e1
        self.e2 = e2

    def render(self, ctx, /):
        ctx.write('(', self.e1, ' == ', self.e2, ')')

class Context:
    __slots__ = ('out', 'w', 'ilv', 'vars_by_id', 'visible_vars', 'aliases')

    def __init__(self, /):
        self.out = []
        self.w = self.out.append
        self.ilv = 0
        self.vars_by_id = {}
        self.visible_vars = set()
        self.aliases = {}

    def indent(self, /):
        self.ilv += 1

    def dedent(self, /):
        self.ilv -= 1

    def stmt_begin(self, /):
        self.w('    ' * self.ilv)

    def stmt_end(self, /):
        self.w('\n')

    def write(self, /, *a):
        w = self.w
        for a in a:
            if a is None:
                continue
            if type(a) is str:
                w(a)
                continue
            a.render(self)

    def done(self):
        return ''.join(self.out)

class GenDist:
    __slots__ = ('stmt_probs',)

    def __init__(self, /, stmt_probs):
        self.stmt_probs = (*([name,p0,pd] for [name,p0,pd] in stmt_probs),)

    def clone(self, /):
        return GenDist(self.stmt_probs)

    def rand_phi(self, /):
        r = 1
        while random_bits(3) <= 2:
            r += 1
        return r

    def rand_range(self, /):
        return random_poisson(3, min=2, max=4)

    def rand_var(self, vs, /):
        n = len(vs)
        r = 0.0
        for i in range(n):
            r += 2**-54 / (i + 1)
        r *= random_bits(54)
        for i,v in enumerate(vs):
            r -= 1 / (i + 1)
            if r <= 0:
                return v
        raise RuntimeError('wtf')

    def rand_expr(self, vs, /, *, mod=None):
        vl = []
        if vs and random_bits(3):
            vs = {*vs}
            for _ in range(random_poisson(1, min=1, max=min(8, len(vs)))):
                v = self.rand_var(vs)
                vl.append(v)
                vs.remove(v)
        if mod is None and len(vl) == 1 and random_bits(1):
            return vl[0]
        if not vl:
            if mod is None:
                mod = 10007
            return ConstExpr(random_int(0, mod))
        return MixExpr(vars=vl, seed=random_bits(64), mod=mod)

    def rand_loc(self, locs, /):
        return random_choice([*locs.keys()])

    def rand_stmt(self, /):
        sp = self.stmt_probs
        r = random_bits(54) / 2**54 * (1 + sum(l[1] for l in sp))
        for l in sp:
            r -= l[1]
            if r < 0:
                l[1] *= l[2]
                return l[0]
        return None

    def rand_acq(self, /):
        return False
        return not random_bits(1)

    def rand_rel(self, /):
        return False
        return not random_bits(1)

    def rand_sub(self, n):
        for l in self.stmt_probs:
            l[1] *= l[2]
        r = self.clone()
        if n > 1:
            for l in self.stmt_probs:
                l[1] *= l[2]
        return r

class Program:
    __slots__ = ('locs', 'threads',)

    def __init__(self, /, *, locs, threads):
        self.locs = locs
        self.threads = threads

    def __str__(self):
        ctx = Context()
        for loc in self.locs:
            ctx.stmt_begin()
            ctx.write('loc ', loc, ';')
            ctx.stmt_end()
        for tn,tb in self.threads.items():
            ctx.write('\n')
            ctx.stmt_begin()
            ctx.write('run ', tb)
            ctx.stmt_end()
        return ctx.done()

    def clone(self):
        return Program(
            locs = {**self.locs},
            threads = {tn: tb.clone(None) for tn,tb in self.threads.items()}
        )

    def nudge(self):
        r = self.clone()
        total = 0
        for tb in r.threads.values():
            n = random_poisson(2, min=1, max=10)
            for i in range(n):
                if not nudge_blk(tb):
                    break
            else:
                i = n
            total += i

        return (total, r if total else None)

def nudge_blk(blk):
    bb = blk.body
    n = len(bb)
    if n <= 1:
        return False

    def try_reorder(i):
        if not can_reorder(bb[i], bb[i+1]):
            return False
        bb[i],bb[i+1] = bb[i+1],bb[i]
        return True

    def try_merge(i):
        a = bb[i]
        b = bb[i+1]
        if type(a) is WriteStmt and type(b) is WriteStmt and a.loc == b.loc:
            del bb[i]
            if a.is_rel:
                b.is_rel = True
            return True
        if type(a) is ReadStmt and type(b) is (ReadStmt, UpdateStmt) and a.loc == b.loc:
            del bb[i]
            if a.is_acq:
                b.is_acq = True
            blk.add_alias(a.var, b.var)
            return True
        if type(a) is WriteStmt and type(b) is ReadStmt and a.loc == b.loc:
            del bb[i+1]
            blk.add_alias(b.var, a.val)
            return True
        if type(a) is WriteStmt and type(b) is UpdateStmt and a.loc == b.loc:
            del bb[i:i+2]
            blk.add_alias(b.var, a.val)
            c = blk.insert(i, WriteStmt, loc=b.loc, val=b.val, is_rel=a.is_rel or b.is_rel)
            return True
        if type(a) is ScStmt and type(b) in (AcqStmt, RelStmt, ScStmt):
            del bb[i+1]
            return True
        if type(a) in (AcqStmt, RelStmt) and type(b) is ScStmt:
            del bb[i]
            return True
        if type(a) is AcqStmt and type(b) is AcqStmt:
            del bb[i+1]
            return True
        if type(a) is RelStmt and type(b) is RelStmt:
            del bb[i+1]
            return True
        return False

    def try_subnudge(i):
        b = bb[i]
        assert type(b) is IfStmt
        subs = [s for _,s in b.conds]
        random_shuffle(subs)
        for s in subs:
            if nudge_blk(s):
                return True
        return False

    poss = [
        *((try_reorder, i) for i in range(n - 1)),
        *((try_merge, i) for i in range(n - 1)),
        *((try_subnudge, i) for i in range(n) if type(bb[i]) is IfStmt),
    ]
    random_shuffle(poss)
    for f,i in poss:
        if f(i):
            return True
    return False

def locs_of(s):
    t = type(s)
    if t in (AcqStmt, RelStmt, ScStmt):
        return frozenset()
    if t in (ReadStmt, WriteStmt, UpdateStmt):
        return frozenset((s.loc,))
    if t is IfStmt:
        r = set()
        for _,b in s.conds:
            for s in b.body:
                r.update(locs_of(s))
        return frozenset(r)
    raise TypeError(t)

def freevars_expr(s):
    r = []
    def rec(e):
        t = type(e)
        if t is ConstExpr:
            return
        if t is Var:
            r.append(e)
            return
        if t is MixExpr:
            for v in e.vars:
                rec(v)
            return
        if t is EqExpr:
            rec(e.e1)
            rec(e.e2)
            return
        raise TypeError(t)
    rec(s)
    return frozenset(r)

def freevars_of(s):
    t = type(s)
    if t in (AcqStmt, RelStmt, ScStmt, ReadStmt):
        return frozenset()
    if t in (WriteStmt, UpdateStmt):
        return freevars_expr(s.val)
    if t is IfStmt:
        r = set()
        for c,b in s.conds:
            if c is not None:
                r.update(freevars_expr(c))
            for s2 in b.body:
                r.update(freevars_of(s2))
            for v,e in b.phi_edges:
                r.update(freevars_expr(e))
            if b.returns is not None:
                r.update(b.returns)
        r -= {*s.phi}
        return frozenset(r)
    raise TypeError(t)

def is_acq(s):
    t = type(s)
    if t in (AcqStmt, ScStmt):
        return True
    if t in (WriteStmt, RelStmt):
        return False
    if t in (ReadStmt, UpdateStmt):
        return s.is_acq
    if t is IfStmt:
        return any(is_acq(s) for _,b in s.conds for s in b.body)
    raise TypeError(t)

def is_rel(s):
    t = type(s)
    if t in (RelStmt, ScStmt):
        return True
    if t in (ReadStmt, AcqStmt):
        return False
    if t in (WriteStmt, UpdateStmt):
        return t.is_rel
    if t is IfStmt:
        return any(is_rel(s) for _,b in s.conds for s in b.body)
    raise TypeError(t)

def can_reorder(a, b):
    ta = type(a)
    tb = type(b)

    #if type(a) is IfStmt or type(b) is IfStmt:
    #    return False

    if locs_of(a) & locs_of(b):
        return False
    if is_acq(a):
        return False
    if is_rel(b):
        return False

    if ta in (ReadStmt, UpdateStmt) and tb is AcqStmt:
        return False

    return not (frozenset(a.vars) & freevars_of(b))

DIST0 = GenDist([
    ['read', 2, 0.7],
    ['update', 2, 0.5],
    ['write', 10, 0.2],
    ['fence', 3, 0.6],
    ['if', 2, 0.5],
])

class Gen:
    __slots__ = ('locs',)

    def __init__(self, n_locs=None):
        if n_locs is None:
            n_locs = random_poisson(3, min=2, max=5)

        if n_locs >= 4:
            max_val = 3
        elif n_locs == 3:
            max_val = 4
        else:
            max_val = 5
        self.locs = {
            f'l{i}': random_poisson(3, min=2, max=max_val)
            for i in range(n_locs)
        }

    def gen_block(self, dist, blk):
        while True:
            op = dist.rand_stmt()
            if op is None:
                break
            if op == 'read':
                blk.add(ReadStmt, loc=dist.rand_loc(self.locs), is_acq=dist.rand_acq())
                continue
            if op == 'write':
                loc = dist.rand_loc(self.locs)
                val = dist.rand_expr(blk.visible_vars(), mod=self.locs[loc])
                blk.add(WriteStmt, loc=loc, val=val, is_rel=dist.rand_rel())
                continue
            if op == 'update':
                loc = dist.rand_loc(self.locs)
                val = lambda v,vv: dist.rand_expr([v, *vv], mod=self.locs[loc])
                blk.add(UpdateStmt, loc=loc, val=val, is_acq=dist.rand_acq(), is_rel=dist.rand_rel())
                continue
            if op == 'fence':
                fkind = random_choice((AcqStmt, RelStmt, ScStmt,))
                blk.add(fkind)
                continue
            if op == 'if':
                vs = blk.visible_vars()
                n_cases = dist.rand_range()
                n_phi = dist.rand_phi()
                dsub = dist.rand_sub(n_cases)
                cond = dist.rand_expr(vs, mod=n_cases)
                cases = [EqExpr(cond, ConstExpr(i)) for i in range(n_cases - 1)]
                def gen(blk, i):
                    dist = dsub.clone()
                    self.gen_block(dist, blk)
                    vs = blk.visible_vars()
                    return [dist.rand_expr(vs) for _ in range(n_phi)]
                blk.add(IfStmt, n_phi=n_phi, cases=cases, gen_case=gen)
                continue
            raise RuntimeError(f'bad op: {op}')

    def gen_thread(self, dist):
        dist = dist.clone()
        root = Block(None)
        self.gen_block(dist, root)
        root.returns = dist.rand_expr(root.visible_vars())
        return root

    def gen_program(self):
        n_locs = len(self.locs)
        if n_locs >= 4:
            max_threads = 3
        elif n_locs == 3:
            max_threads = 4
        else:
            max_threads = 5
        n_threads = random_poisson(3, min=2, max=max_threads)
        return Program(
            locs = self.locs,
            threads = {
                f'thr{i}': self.gen_thread(DIST0)
                for i in range(n_threads)
            },
        )

def main():
    def out_set(s):
        return frozenset(s.split(b'\n'))

    def run(inp):
        inp = f'{inp}'.encode()
        p = subprocess.Popen(['./target/release/umm', '-t4'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            out,err = p.communicate(inp, timeout=1)
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()
            raise
        if p.returncode != 0:
            raise RuntimeError(f'failed: {err.decode()}')
        return out

    while True:
        try:
            prog1 = Gen().gen_program()
            nn, prog2 = prog1.nudge()
            if not nn:
                continue

            try:
                o1 = run(prog1)
                o2 = run(prog2)
            except subprocess.TimeoutExpired:
                print('timeout')
                continue
            if o1 == o2:
                print(f'no diff, {nn} code changes')
                continue
            o1 = out_set(o1)
            o2 = out_set(o2)
            if o1 == o2:
                print(f'no diff, {nn} code changes')
                continue
            if not (o1 >= o2):
                print(o1, o2)
                raise RuntimeError('test failed')
            print(f'diff, {nn} code changes')
        except:
            with io.open('tstfail-1', 'w') as f:
                f.write(str(prog1))
            with io.open('tstfail-2', 'w') as f:
                f.write(str(prog2))
            raise

main()
