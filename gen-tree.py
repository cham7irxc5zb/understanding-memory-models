import math, random, io

class Step:
    __slots__ = ('next',)
    def __init__(self):
        self.next = None

class State:
    __slots__ = ()

class Read(State):
    __slots__ = ('loc', 'read')

class Write(State):
    __slots__ = ('loc', 'val', 'step')

class Env:
    __slots__ = ('locs',)

def sim(mem, threads, qr, qq):
    key = (*mem,*threads)
    if key in qq:
        return
    qq.add(key)

    all_ret = True
    for i,s in enumerate(threads):
        q = s.next
        t = type(q)
        if t is int:
            continue
        all_ret = False
        threads2 = [*threads]
        if t is Read:
            loc = q.loc
            s,u = q.read[mem[loc]]
            threads2[i] = s
            if u is None:
                mem2 = mem
            else:
                mem2 = [*mem]
                mem2[loc] = u
            sim(mem2, threads2, qr, qq)
        elif t is Write:
            loc = q.loc
            mem2 = [*mem]
            mem2[loc] = q.val
            threads2[i] = q.step
            sim(mem2, threads2, qr, qq)
        else:
            raise TypeError

    if all_ret:
        qr.add((*(s.next for s in threads),))

class Gen:
    __slots__ = ('op_limit',)

    def rand_n_locs(self):
        raise NotImplementedError

    def rand_n_vals(self, loc):
        raise NotImplementedError

    def rand_n_threads(self):
        raise NotImplementedError

    def rand_sc(self):
        return random.randrange(0, 100)

    def new(self, out):
        n_locs = self.rand_n_locs()
        locs = [self.rand_n_vals(i) for i in range(n_locs)]

        n_threads = self.rand_n_threads()
        threads = [Step() for _ in range(n_threads)]

        free = [*threads]

        def rand_val(nvals, nnone):
            r = random.randrange(nvals + nnone)
            if r >= nvals:
                return None
            return r

        def mkread(nvals, genval):
            for i in range(nvals):
                s = Step()
                free.append(s)
                yield (s, genval(i))

        def gen_read():
            loc = random.randrange(0, len(locs))
            nvals = locs[loc]
            r = Read()
            r.loc = loc
            r.read = (*mkread(nvals, lambda _: None),)
            return r

        def gen_update():
            loc = random.randrange(0, len(locs))
            nvals = locs[loc]
            r = Read()
            r.loc = loc
            r.read = (*mkread(nvals, lambda _: rand_val(nvals, 1)),)
            return r

        def gen_write():
            loc = random.randrange(0, len(locs))
            nvals = locs[loc]
            s = Step()
            free.append(s)
            r = Write()
            r.loc = loc
            r.val = rand_val(nvals, 0)
            r.step = s
            return r

        while True:
            ops = (
                [self.rand_n_reads(), gen_read],
                [self.rand_n_writes(), gen_write],
                [self.rand_n_updates(), gen_update],
            )
            if sum(w for w,o in ops) <= self.op_limit:
                break

        while sum(w for w,o in ops):
            [o] = random.choices(ops, weights=[w for w,o in ops])
            o[0] -= 1
            o = o[1]

            i = random.randrange(0, len(free))
            s = free[i]
            free[i] = free[-1]
            free.pop()
            s.next = o()

        for i in range(n_locs):
            out(f'loc l{i};')

        rctr = None
        def reg():
            nonlocal rctr
            r = rctr
            rctr = r + 1
            return r

        def dump_state(q):
            t = type(q)
            if t is Read:
                r = reg()
                if all(u is None for s,u in q.read):
                    out(f'reg r{r} = read l{q.loc};')
                else:
                    elsepfx = f'reg r{r}; update l{q.loc} r{r} '
                    for i,(s,u) in enumerate(q.read[:-1]):
                        out(f'{elsepfx}if r{r} == {i} {{')
                        out('break' if u is None else f'{u}')
                        elsepfx = '} else '
                    s,u = q.read[-1]
                    out(f'{elsepfx}{{')
                    out('break' if u is None else f'{u}')
                    out('};')

                elsepfx = ''
                for i,(s,u) in enumerate(q.read[:-1]):
                    out(f'{elsepfx}if r{r} == {i} {{')
                    dump_step(s)
                    elsepfx = '} else '
                s,u = q.read[-1]
                out(f'{elsepfx}{{')
                dump_step(s)
                out('}')

            elif t is Write:
                out(f'write l{q.loc} {q.val};')
                dump_step(q.step)

            else:
                raise TypeError

        vctr = 0
        def dump_step(s):
            nonlocal vctr
            if type(s) is not Step:
                raise TypeError
            q = s.next
            if q is None:
                s.next = q = vctr
                vctr = q + 1
                out(f'{q}')
                return
            thresh = sorted(random.randrange(0, 100) for _ in range(3))
            out(f'if @SCT@ > {thresh[2]} {{ sc }}')
            out(f'else if @SCT@ > {thresh[1]} {{ acq; rel; }}')
            if random.randrange(2):
                out(f'else if @SCT@ > {thresh[0]} {{ acq; }}')
            else:
                out(f'else if @SCT@ > {thresh[0]} {{ rel; }}')
            dump_state(q)

        threads = [t for t in threads if t.next is not None]

        for t in threads:
            out(f'run {{')
            rctr = 0
            vctr = 0
            dump_state(t.next)
            out('}')

        simres = set()
        sim([0] * n_locs, threads, simres, set())
        return simres

def random_poisson(avg, min=0, max=None):
    while True:
        avg /= 1
        scale = math.exp(avg)

        v = 1.0
        start = 0.0
        i = 0
        while i < min:
            scale -= v
            i += 1
            v *= avg / i

        scale *= random.random()
        while max is None or i <= max:
            if scale <= v:
                return i
            scale -= v
            i += 1
            v *= avg / i

class PoissonGen(Gen):
    __slots__ = ('min_n_locs', 'max_n_locs', 'avg_n_locs', 'avg_n_vals', 'max_n_vals', 'avg_n_threads', 'max_n_threads', 'avg_n_reads', 'avg_n_writes', 'avg_n_updates')

    def __init__(self):
        self.op_limit = 18

        self.min_n_locs = 2
        self.max_n_locs = 4
        self.avg_n_locs = 3
        self.avg_n_vals = 2
        self.max_n_vals = 3
        self.avg_n_threads = 2
        self.max_n_threads = 4

        self.avg_n_reads = 6
        self.avg_n_writes = 4
        self.avg_n_updates = 3

    def rand_n_locs(self):
        return random_poisson(self.avg_n_locs, min=self.min_n_locs, max=self.max_n_locs)

    def rand_n_vals(self, i):
        return random_poisson(self.avg_n_vals, min=2, max=self.max_n_vals)

    def rand_n_threads(self):
        return random_poisson(self.avg_n_threads, min=2, max=self.max_n_threads)

    def rand_n_reads(self):
        return random_poisson(self.avg_n_reads, min=3)

    def rand_n_writes(self):
        return random_poisson(self.avg_n_writes, min=3, max=8)

    def rand_n_updates(self):
        return random_poisson(self.avg_n_updates, max=6)

def main():
    while True:
        r = PoissonGen()
        tst = io.StringIO()
        vals = r.new(lambda l: tst.write(l + '\n'))
        if len(vals) > 1 and len(vals) < 100:
            assert vals
            vals = sorted(' '.join(map(str, v)) for v in vals)
            tst = tst.getvalue()
            break

    with io.open('run-test/template', 'w') as f:
        f.write(tst)
    with io.open('run-test/o-sc', 'w') as f:
        f.write('\n'.join(vals))
        f.write('\n')

main()
