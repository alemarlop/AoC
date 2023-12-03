const GEAR_SYMBOL = '*';
const GEAR_ADJACENTS = 2;
const DIRECTION_OPS = {
    LEFT: ([y, x]: [number, number]) => [y, x-1] as [number, number],
    RIGHT: ([y, x]: [number, number]) => [y, x+1] as [number, number],

    TOP: ([y, x]: [number, number]) => [y-1, x] as [number, number],
    BOTTOM: ([y, x]: [number, number]) => [y+1, x] as [number, number],

    TOP_LEFT: ([y, x]: [number, number]) => [y-1, x-1] as [number, number],
    TOP_RIGHT: ([y, x]: [number, number]) => [y-1, x+1] as [number, number],

    BOTTOM_LEFT: ([y, x]: [number, number]) => [y+1, x-1] as [number, number],
    BOTTOM_RIGHT: ([y, x]: [number, number]) => [y+1, x+1] as [number, number],
};
const ALL_DIRECTIONS_OPS = [
    DIRECTION_OPS.BOTTOM, DIRECTION_OPS.BOTTOM_LEFT, DIRECTION_OPS.BOTTOM_RIGHT, DIRECTION_OPS.LEFT,
    DIRECTION_OPS.RIGHT, DIRECTION_OPS.TOP, DIRECTION_OPS.TOP_LEFT, DIRECTION_OPS.TOP_RIGHT
]


const fileName: string = Deno.args[0]
const rawText: string = await Deno.readTextFile(fileName)
const matrix: string[][] = rawText
    .split('\n')
    .map(line => line.split(''));

console.log('Part 1:', sumRelevantNumbers(getNumberMap(rawText, matrix)));
console.log('Part 2:', sumGears(getNumberMap(rawText, matrix), matrix));

function sumRelevantNumbers(symbolMap: Map<[number, number], Set<number>>): number {
    return [...symbolMap.keys()].reduce((sum, currentSymbol) => {
        return sum + [...symbolMap.get(currentSymbol)?.values() as IterableIterator<number>]
            .reduce((localSum, localCurrent) => localCurrent + localSum, 0);
    }, 0);
}

function sumGears(symbolMap: Map<[number, number], Set<number>>, matrix: string[][]): number {
    const allSymbols = [...symbolMap.keys()];
    const gears = allSymbols.filter(symbolPosition => {
        return symbolMap.get(symbolPosition)?.size == GEAR_ADJACENTS && matrix[symbolPosition[0]][symbolPosition[1]] === GEAR_SYMBOL;
    });
    return gears.reduce((sum, current) => {
        return sum + [...(symbolMap.get(current) as Set<number>).values()].reduce((mult, localCurr) => mult *= localCurr, 1);
    }, 0)
}

function getNumberMap(rawText: string, matrix: string[][]): Map<[number, number], Set<number>> {
    const allSymbols = (rawText.match(/[^(\d|\.|\n)]/g) ?? []) as string[];
    const symbolMap: Map<[number, number], Set<number>> = new Map();
    matrix.forEach((line, y) => {
        line.forEach((cell, x) => {
            if (allSymbols.includes(cell)) symbolMap.set([y,x], new Set());
        })
    });
    [...symbolMap.keys()].forEach(symbol => checkAdjacents(symbol, symbolMap, matrix));
    return symbolMap;
}

function checkAdjacents(position: [number, number], symbolMap: Map<[number, number], Set<number>>, matrix: string[][]): Map<[number, number], Set<number>> {
    for (const op of ALL_DIRECTIONS_OPS) {
        const slidingPosition = op([...position]) as [number, number];
        if (matrix[slidingPosition[0]][slidingPosition[1]].match(/\d/)) {
            const completeNumber = getCompleteNumber(matrix, slidingPosition);
            const currentValue = (symbolMap.get(position) as Set<number>);
            currentValue.add(completeNumber);
            symbolMap.set(position, currentValue)
        };
    }
    return symbolMap;
}

function getCompleteNumber(matrix: string[][], position: [number, number]): number {
    const startingElement = matrix[position[0]][position[1]];
    const right = completeNumberDirection(DIRECTION_OPS.RIGHT, matrix, position);
    const left = completeNumberDirection(DIRECTION_OPS.LEFT, matrix, position);
    return Number(`${left.split('').reverse().join('')}${startingElement}${right}`);
}

function completeNumberDirection(direction: ([y, x]: [number, number]) => [number, number], matrix: string[][], position: [number, number]) {
    let result = "";
    let currentPosition = [...position] as [number, number];
    let point = matrix[currentPosition[0]][currentPosition[1]];

    while (/\d/.test(point) && !!point) {
        result += matrix[currentPosition[0]][currentPosition[1]];
        currentPosition = direction(currentPosition);
        point = matrix[currentPosition[0]][currentPosition[1]];
    }
    return result.slice(1);
}