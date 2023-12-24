const fileName: string = 'input.txt';
// const fileName: string = Deno.args[0];
const file = await Deno.readTextFile(fileName);

const slopes: any = {
    '>': (x: number, y: number) => [x + 1, y],
    '<': (x: number, y: number) => [x - 1, y],
    '^': (x: number, y: number) => [x, y - 1],
    'v': (x: number, y: number) => [x, y + 1],
};

const operations: any = {
    'right': (x: number, y: number) => [x + 1, y],
    'left': (x: number, y: number) => [x - 1, y],
    'up': (x: number, y: number) => [x, y - 1],
    'down': (x: number, y: number) => [x, y + 1],
};

const map = file.split('\n').map((row) => row.split(''));
const memo = new Map<string, Set<Set<string>>>();
const memo2 = new Map<string, number>();
console.log('Part 1:', findLongestPath(map, [1, 0], [map.length - 2, map.length - 1], new Set(), true).length);
console.log('Part 2:', findLongestPath(map, [1, 0], [map.length - 2, map.length - 1], new Set(), false).length);

function findLongestPath(map: string[][], startingPoint: [number, number], endingPoint: [number, number], visitedCells: Set<string>, slopesEnabled: boolean): [number, number][] {
    let count: [number, number][] = [];
    let currentPoint = [...startingPoint];
    while (currentPoint[0] !== endingPoint[0] || currentPoint[1] !== endingPoint[1]) {
        const [x, y] = currentPoint;
        if (visitedCells.has(JSON.stringify(currentPoint))) {
            return [];
        }
        visitedCells.add(JSON.stringify(currentPoint));
        count.push(currentPoint as [number, number]);

        const currentChar = map[y][x];

        // current cell is a slope
        if ([...Object.keys(slopes)].includes(currentChar) && slopesEnabled) {
            const [newX, newY] = slopes[currentChar](x, y);
            currentPoint = [newX, newY];
            continue;
        }

        // calculate next possitions
        const possibleNextPoints = [];
        for (const key of Object.keys(operations)) {
            const next = operations[key](x, y);
            if (!map[next[1]] || map[next[1]][next[0]] === '#' || visitedCells.has(JSON.stringify(next))) continue;
            possibleNextPoints.push(next);
        }

        // if there is only one possible next point, go there
        if (possibleNextPoints.length === 1) {
            currentPoint = possibleNextPoints[0];
            continue;
        }

        // if there are more than one possible next points, try both
        if (possibleNextPoints.length > 1) {
            if (memo2.has(JSON.stringify(currentPoint))) {
                const visitedCellsSize = memo2.get(JSON.stringify(currentPoint)) as number;
                // if (visitedCellsSize > visitedCells.size + 5000) {
                //     return [];
                // }
            }
            if (memo.has(JSON.stringify(currentPoint)) && (memo.get(JSON.stringify(currentPoint)) as Set<Set<string>>).size > 9000) {
                const set = memo.get(JSON.stringify(currentPoint)) as Set<Set<string>>;
                let viable = [...set.values()].filter((path) => {
                    for (const cell of path) {
                        if (visitedCells.has(cell)) return false;
                    }
                    return true;
                });
                if (viable.length > 0) {
                    viable = viable.sort((a, b) => b.size - a.size);
                    const path = viable[0];
                    count = count.concat([...path].map((cell) => JSON.parse(cell)));
                    break;
                }
            }
            const counts: Map<number, Set<string>> = new Map<number, Set<string>>();
            for (const nextPoint of possibleNextPoints) {
                const newVisitedCells = new Set([...visitedCells]);
                const path = findLongestPath(map, nextPoint, endingPoint, newVisitedCells, slopesEnabled);
                counts.set(path.length, new Set(path.map((cell) => JSON.stringify(cell))));
            }
            const max = Math.max(...counts.keys());
            if (!memo.has(JSON.stringify(currentPoint))) {
                memo.set(JSON.stringify(currentPoint), new Set([new Set(counts.get(max))]));
                memo2.set(JSON.stringify(currentPoint), visitedCells.size);
            } else if (max !== 0) {
                const set = memo.get(JSON.stringify(currentPoint)) as Set<Set<string>>;
                memo.set(JSON.stringify(currentPoint), set.add(new Set(counts.get(max))));
                memo2.set(JSON.stringify(currentPoint), Math.max(visitedCells.size, memo2.get(JSON.stringify(currentPoint)) as number));
            }
            count = count.concat([...counts.get(max) as Set<string>].map((cell) => JSON.parse(cell)));
            break;
        }

        return [];

    }
    return count;
}