enum Direction {
    UP = 'U',
    DOWN = 'D',
    LEFT = 'L',
    RIGHT = 'R'
};

interface Dig {
    direction: Direction | Direction2;
    distance: number;
    color: string;
}


const operations: { [key in Direction]: (...args: number[]) => number[] } = {
    [Direction.UP]: (x: number, y: number, distance: number): number[] => [x, y + distance],
    [Direction.DOWN]: (x: number, y: number, distance: number): number[] => [x, y - distance],
    [Direction.LEFT]: (x: number, y: number, distance: number): number[] => [x - distance, y],
    [Direction.RIGHT]: (x: number, y: number, distance: number): number[] => [x + distance, y],
};

const fileName: string = Deno.args[0];
const fileContent = await Deno.readTextFile(fileName);

const lines: string[] = fileContent.split('\n');

const digs: Dig[] = lines.map((line: string) => {
    const [direction, distance, color] = line.split(' ');
    return {
        direction: direction as Direction,
        distance: Number(distance),
        color
    };
});

const digs2: Dig[] = digs.map((dig: Dig) => {
    let direction = dig.direction;
    switch (Number(dig.color[dig.color.length - 2]))
    {
        case 0:
            direction = Direction.RIGHT;
            break;
        case 1:
            direction = Direction.DOWN;
            break;
        case 2:
            direction = Direction.LEFT;
            break;
        case 3:
            direction = Direction.UP;
            break;
    }
    return {
        direction,
        color: '',
        distance: parseInt(dig.color.substring(2, dig.color.length - 2), 16),
    } as Dig;
});

// function traceMap(digs: Dig[]): number[][] {
//     const map: number[][] = [[0,0]];
//     let currentPosition: [number, number] = [0,0];
//     digs.forEach((dig: Dig) => {
//         const { direction, distance, color } = dig;
//         for (let i = 0; i < distance; i++) {
//             currentPosition = operations[direction](...currentPosition) as [number, number];
//             map.push(currentPosition);
            
//         }
//     });
//     return map;
// }

function traceVertex(digs: Dig[]): [number, number][] {
    let currentPosition: [number, number] = [0,0];
    const vertex: [number, number][] = [currentPosition];
    digs.forEach((dig: Dig) => {
        const { direction, distance, color } = dig;
        vertex.push(operations[direction](...currentPosition, distance) as [number, number]);
        currentPosition = vertex[vertex.length - 1];
    });
    return vertex;
}

function calculateEnclosedArea(digs: Dig[]): number {
    const vertex: [number, number][] = traceVertex(digs);
    //shoelace formula
    let area = 0;
    for (let i = 0; i < vertex.length - 1; i++) {
        area += vertex[i][0] * vertex[i + 1][1] - vertex[i][1] * vertex[i + 1][0];
    }
    return Math.abs(area / 2);
}


// console.log(traceVertex(digs));
console.log('Part 1:', (calculateEnclosedArea(digs) + (digs.reduce((acc, dig) => acc + dig.distance, 0)/2) + 1));
console.log('Part 2:', (calculateEnclosedArea(digs2) + (digs2.reduce((acc, dig) => acc + dig.distance, 0)/2) + 1));
