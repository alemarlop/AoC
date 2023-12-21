const MOVEMENTS = {
    UP: (x: number, y: number) => [x, y - 1],
    DOWN: (x: number, y: number) => [x, y + 1],
    LEFT: (x: number, y: number) => [x - 1, y],
    RIGHT: (x: number, y: number) => [x + 1, y]
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);

const matrix: string[][] = fileContent.split('\n').map((row) => row.split(''));
const startingPoint: [number, number] = matrix.reduce((acc, row, y) => {
    const x = row.indexOf('S');
    return x !== -1 ? [x, y] : acc;
}, [-1, -1]);

function possibleEnds(matrix: string[][], startingPoint: [number, number], totalSteps: number): number {
    let steps = 0;
    const xLength = matrix[0].length;
    const yLength = matrix.length;

    let currentSteps: Set<string> = new Set([JSON.stringify(startingPoint)]);
    let nextSteps: [number, number][] = [];

    while (steps < totalSteps) {
        nextSteps = [];
        for (const step of currentSteps) {
            const [x, y] = JSON.parse(step);
            for (const [_direction, movement] of Object.entries(MOVEMENTS)) {
                const [nextX, nextY] = movement(x, y);
                const [nextXi, nextYi] = [((nextX % xLength) + xLength) % xLength, ((nextY % yLength) + yLength) % yLength];
                if ((matrix[nextYi]?.[nextXi] === '.' || matrix[nextYi]?.[nextXi] === 'S')  && !currentSteps.has(JSON.stringify([nextX, nextY]))) {
                    nextSteps.push([nextX, nextY]);
                }
            }
        }
        currentSteps = new Set(nextSteps.map(([x, y]) => JSON.stringify([x, y])));
        // console.log(steps, currentSteps.size)
        steps++;
    }
    return currentSteps.size;
}

function lagrange(...points: [number, number][]): (x: number) => number {
    const n = points.length;
    return (x: number) => {
        let result = 0;
        for (let i = 0; i < n; i++) {
            let term = points[i][1];
            for (let j = 0; j < n; j++) {
                if (i !== j) {
                    term *= (x - points[j][0]) / (points[i][0] - points[j][0]);
                }
            }
            result += term;
        }
        return result;
    }
}

console.log('Part 1:', possibleEnds(matrix, startingPoint, 64));
console.log('Part 2:', getPart2(matrix, startingPoint, 26501365));

function getPart2(matrix: string[][], startingPoint: [number, number], steps: number) {
    const l1 = [65, possibleEnds(matrix, startingPoint, 65)] as [number, number];
    const l2 = [65 + 131, possibleEnds(matrix, startingPoint, 65 + 131)] as [number, number];
    const l3 = [65 + 2 * 131, possibleEnds(matrix, startingPoint, 65 + 2 * 131)] as [number, number];
    return Math.floor(lagrange(l1, l2, l3)(steps));
}
