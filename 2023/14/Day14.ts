enum Directions {
    NORTH,
    WEST,
    SOUTH,
    EAST,
}
const DIRECTION_OPS = {
    [Directions.NORTH]: (y: number, x: number) => [y - 1, x],
    [Directions.WEST]: (y: number, x: number) => [y, x - 1],
    [Directions.SOUTH]: (y: number, x: number) => [y + 1, x],
    [Directions.EAST]: (y: number, x: number) => [y, x + 1],
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);
const matrix = fileContent.split('\n').map((row) => row.split(''));

console.log('Part 1:', converge(matrix, Directions.NORTH).reduce((a, b, i) => a + b.reduce((c, d) => c + (d === 'O' ? 1 : 0), 0) * (matrix.length - i), 0));
console.log('Part 2:', runCycles(matrix, 1000000000, true).reduce((a, b, i) => a + b.reduce((c, d) => c + (d === 'O' ? 1 : 0), 0) * (matrix.length - i), 0));


function runCycles(matrix: string[][], cycles: number, useCache: boolean): string[][] {
    const cache = new Map<string, number>();
    let newMatrix = matrix.map((row) => [...row]);
    for (let i = 0; i < cycles; i++) {
        for (const dir of [Directions.NORTH, Directions.WEST, Directions.SOUTH, Directions.EAST]) {
            newMatrix = converge(newMatrix, dir);
        }
        const matrixString = JSON.stringify(newMatrix);
        if (cache.has(matrixString) && useCache) {
            const offsetStart = cache.get(matrixString) as number;
            const cycleLength = i - offsetStart;
            const remainingCycles = cycles - i - 1;
            const offset = remainingCycles % cycleLength;
            return runCycles(newMatrix, offset, false);
        } else if (useCache) {
            cache.set(matrixString, i);
        }
    }
    return newMatrix;
}

function converge(matrix: string[][], direction: Directions): string[][] {
    while (true) {
        const matrixString = JSON.stringify(matrix);
        const nextMatrix = nextStep(matrix, direction);
        if (matrixString === JSON.stringify(nextMatrix)) break;
        matrix = nextMatrix;
    }
    return matrix;
}

function nextStep(matrix: string[][], direction: Directions): string[][] {
    const newMatrix = matrix.map((row) => [...row]);
    for (let y = 0; y < matrix.length; y++) {
        const row = matrix[y];
        for (let x = 0; x < row.length; x++) {
            const cell = row[x];
            if (cell !== 'O') continue;
            const [aboveY, aboveX] = DIRECTION_OPS[direction](y, x);
            const aboveCell = matrix[aboveY]?.[aboveX];
            if (aboveCell !== '.') continue;
            newMatrix[aboveY][aboveX] = 'O';
            newMatrix[y][x] = '.';
        }
    }
    return newMatrix;
}