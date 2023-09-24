const fileName = Deno.args[0];
const input = await Deno.readTextFile(fileName);
const steps = input.split(',').map(step => step.trim());
const rotations = {
    'N': {'R': 'E', 'L': 'W'},
    'E': {'L': 'N', 'R': 'S'},
    'S': {'L': 'E', 'R': 'W'},
    'W': {'L': 'S', 'R': 'N'}
} as Record<string, Record<string, string>>;
const movements = {
    'N': (from: [number, number], distance: number) => [from[0], from[1] + distance],
    'E': (from: [number, number], distance: number) => [from[0] + distance, from[1]],
    'S': (from: [number, number], distance: number) => [from[0], from[1] - distance],
    'W': (from: [number, number], distance: number) => [from[0] - distance, from[1]]
} as Record<string, (from: [number, number], distance: number) => [number, number]>;

const initialPosition: [number, number] = [0, 0];
let start = performance.now();
let endPosition = walkAll('N', initialPosition);
let end = performance.now();
console.log(`Part 1: ${Math.abs(endPosition[0] - initialPosition[0]) + Math.abs(endPosition[1] - initialPosition[1])} \t| ${end - start}ms`);

start = performance.now();
endPosition = walkAll('N', initialPosition, true);
end = performance.now();
console.log(`Part 2: ${Math.abs(endPosition[0] - initialPosition[0]) + Math.abs(endPosition[1] - initialPosition[1])} \t| ${end - start}ms`);

function walkAll(direction: string, initialPosition: [number, number], storeRoute?: boolean): [number, number] {
    let currentPosition: [number, number] = [...initialPosition];
    const visited: {from: [number, number], to: [number, number]}[] = [];
    for (const step of steps) {
        const relativeDirection = step[0];
        const distance = Number(step.slice(1));
        direction = rotations[direction][relativeDirection];
        const nextPosition = movements[direction](currentPosition, distance);
        if (storeRoute) {
            const intersection = checkIntersection(visited, {from: currentPosition, to: nextPosition});
            if (intersection) return intersection;
            visited.push({from: currentPosition, to: nextPosition});

        }
        currentPosition = nextPosition;
    };
    return currentPosition;
}

function checkIntersection(
    store: {from: [number, number], to: [number, number]}[],
    challenger: {from: [number, number], to: [number, number]}): [number, number] | undefined {

    for (const route of store) {
        const {from: [x1, y1], to: [x2, y2]} = route;
        const {from: [x3, y3], to: [x4, y4]} = challenger;
        const denominator = ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1));
        const ua = (((x4 - x3) * (y1 - y3)) - ((y4 - y3) * (x1 - x3))) / denominator;
        const ub = (((x2 - x1) * (y1 - y3)) - ((y2 - y1) * (x1 - x3))) / denominator;
        const x = x1 + (ua * (x2 - x1));
        const y = y1 + (ua * (y2 - y1));
        if (ua > 0 && ua < 1 && ub > 0 && ub < 1) {
            return [x, y];
        }
    };

    return undefined;
}