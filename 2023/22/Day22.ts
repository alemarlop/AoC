interface Brick {
    id: number;
    from: [number, number, number];
    to: [number, number, number];
    occupedCells: [number, number, number][];
}

const fileName: string = Deno.args[0];
const fileContent = await Deno.readTextFile(fileName);

const bricks: Brick[] = fileContent.split('\n').map((line: string, i: number) => {
    const [from, to] = line.split('~');
    const occupedCells: [number, number, number][] = [];
    for (let x = Number(from.split(',')[0]); x <= Number(to.split(',')[0]); x++) {
        for (let y = Number(from.split(',')[1]); y <= Number(to.split(',')[1]); y++) {
            for (let z = Number(from.split(',')[2]); z <= Number(to.split(',')[2]); z++) {
                occupedCells.push([x, y, z]);
            }
        }
    }
    return {
        id: i,
        from: from.split(',').map((n: string) => Number(n)) as [number, number, number],
        to: to.split(',').map((n: string) => Number(n)) as [number, number, number],
        occupedCells,
    };
});

const sortedBricks = bricks.sort((a: Brick, b: Brick) => {
    return Math.max(a.from[2], a.to[2]) - Math.max(b.from[2], b.to[2]);
});

function dropBricks(bricks: Brick[], map: Map<number, Set<number>>): [Brick[], number] {
    let countDropped = 0;
    const result: Brick[] = [];
    while (bricks.length > 0) {
        const brick = bricks.shift();
        const outputBrick = dropBrick(brick as Brick, result, map);
        if (outputBrick.from[2] !== (brick as Brick).from[2]) {
            countDropped++;
        }
        result.push(outputBrick);
    }
    return [result, countDropped]
}

function dropBrick(brick: Brick, bricks: Brick[], map: Map<number, Set<number>>): Brick {
    let currentBrick = brick;
    while (true) {
        if (currentBrick.from[2] === 1 || currentBrick.to[2] === 1) {
            return currentBrick;
        }
        const droppedOne = {
            ...currentBrick,
            from : [currentBrick.from[0], currentBrick.from[1], currentBrick.from[2] - 1],
            to: [currentBrick.to[0], currentBrick.to[1], currentBrick.to[2] - 1],
            occupedCells: currentBrick.occupedCells.map((cell: [number, number, number]) => {
                return [cell[0], cell[1], cell[2] - 1];
            }),
        } as Brick;

        const collision = bricks.filter((b: Brick) => {
            return b.occupedCells.some((cell: [number, number, number]) => {
                return droppedOne.occupedCells.some((droppedCell: [number, number, number]) => {
                    return droppedCell[0] === cell[0] && droppedCell[1] === cell[1] && droppedCell[2] === cell[2];
                });
            });
        });
        if (collision.length > 0) {
            collision.forEach((c: Brick) => map.set(c.id, (map.get(c.id) as Set<number>).add(droppedOne.id)));
            return currentBrick;
        }
        currentBrick = droppedOne;
    }
}

function part1(bricks: Brick[]): number {
    const map = new Map<number, Set<number>>();
    bricks.forEach((brick: Brick) => map.set(brick.id, new Set<number>()));
    dropBricks([...bricks], map);
    const bricks1 =[...map.keys()];
    const holds: number[] = bricks1.filter((brick: number) => {
        const holding = map.get(brick) as Set<number>;
        return holding.size === 0 || [...holding].every((h: number) => {
            const allValues = [...map.values()];
            const present = allValues.filter(v => v.has(h));
            return present.length > 1;
        });
    });
    return holds.length;
}

function part2(bricks: Brick[]): number {
    const map = new Map<number, Set<number>>();
    const result: number[] = [];
    bricks.forEach((brick: Brick) => map.set(brick.id, new Set<number>()));
    const [droppedBricks, _fell] = dropBricks([...bricks], map);
    let count = 0;
    for (const key of map.keys()) {
        const excluded = droppedBricks.filter((b: Brick) => b.id !== key);
        const [_droppedBricks, fell] = dropBricks(excluded, map);
        result.push(fell);
        count++;
        console.log('Tried', count, 'out of', map.size, fell);
    }
    return result.reduce((acc: number, curr: number) => acc + curr, 0);
}


console.log('Part 1:', part1([...sortedBricks]));
console.log('Part 2:', part2([...sortedBricks]));