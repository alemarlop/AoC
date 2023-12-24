import * as math from 'mathjs';
import { AnySort, Arith, Expr, RatNum, init } from 'z3-solver';
const fs = require('fs');

interface Hailstone {
    id: number;
    px: number;
    py: number;
    pz: number;
    vx: number;
    vy: number;
    vz: number;
}

const fileContent = fs.readFileSync('input.txt', 'utf8');

const hailstones: Hailstone[] = fileContent.split('\n').map((line: any, index: any) => {
    const [px, py, pz, vx, vy, vz] = (line.match(/(\-?)\d+/g) as RegExpMatchArray).map(num => Number(num));
    return { id: index, px, py, pz, vx, vy, vz };
});

async function part2() {
    const { Context } = await init();
    const { Solver, Real } = Context('main');
    const vars = {} as Record<'x' | 'y' | 'z' | 'vx' | 'vy' | 'vz', Arith<'main'>>;
    for (const v of ['x', 'y', 'z', 'vx', 'vy', 'vz'] as const) vars[v] = Real.const(v);
    const solver = new Solver();
    for (let i = 0; i < 3; i++) {
        const hailstone = hailstones[i];
        const { origin, direction } = { origin: [hailstone.px, hailstone.py, hailstone.pz], direction: [hailstone.vx, hailstone.vy, hailstone.vz]};
        const tk = Real.const('tk' + i);
        solver.add(tk.mul(direction[0]).add(origin[0]).eq(tk.mul(vars.vx).add(vars.x)));
        solver.add(tk.mul(direction[1]).add(origin[1]).eq(tk.mul(vars.vy).add(vars.y)));
        solver.add(tk.mul(direction[2]).add(origin[2]).eq(tk.mul(vars.vz).add(vars.z)));
    }

    const solved = await solver.check(); // Actually solves the thing
    if (solved === 'unsat') throw new Error('Unable to solve equation');
    const model = solver.model();

    function parseRatNum(expr: Expr<'main', AnySort<'main'>, unknown>): number {
        const value = (expr as RatNum).value();
        const num = Number(value.numerator.toString().replace('n', ''));
        const denom = Number(value.denominator.toString().replace('n', ''));
        return num / denom;
    }
    return parseRatNum(model.eval(vars.x.add(vars.y).add(vars.z)));
}

function countIntersections(hailstones: Hailstone[], from: number, to: number): number {
    const result: [number, number][] = [];
    hailstones.forEach(hailstone => {
        hailstones.forEach(other => {
            if (hailstone.id === other.id) {
                return;
            }
            const [x, y] = intersectsOnXY(hailstone, other) ?? [];
            if (x === undefined || y === undefined) {
                return;
            }
            if (x >= from && x <= to && y >= from && y <= to) {
                result.push([x, y]);
            }
        });
    });
    return result.length / 2;
}

function intersectsOnXY(hailstone: Hailstone, other: Hailstone): [number, number] | undefined {
    if (hailstone.id === other.id) {
        return undefined;
    }
    const c1 = (hailstone.px - other.px) * -1;
    const cx1 = hailstone.vx;
    const cy1 = other.vx * -1;

    const c2 = (hailstone.py - other.py) * -1;
    const cx2 = hailstone.vy;
    const cy2 = other.vy * -1;

    const A = math.matrix([[cx1, cy1], [cx2, cy2]]);
    try {
        const [x1, x2] = math.lusolve(A, [c1, c2]).toArray().map(num => Number(num));
        if (x1 < 0 || x2 < 0) {
            return undefined;
        }
        return [hailstone.px + x1 * hailstone.vx, hailstone.py + x1 * hailstone.vy];
    } catch {
        return undefined;
    }
}

console.log('Part 1:', countIntersections(hailstones, 200000000000000, 400000000000000));
part2().then(x => console.log('Part 2:', x));

