import { TreeNode } from "./TreeNode.ts";

// const fileName: string = 'sample.txt';
const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);
const heatMap: number[][] = fileContent.split('\n').map((row) => row.split('').map((cell) => Number(cell)));

export enum Orientation {
    UP,
    DOWN,
    LEFT,
    RIGHT
}

enum Direction {
    STRAIGHT,
    LEFT,
    RIGHT
}

const ORIENTATION_DIRECTION_MAP = {
    [Orientation.UP]: {
        [Direction.STRAIGHT]: Orientation.UP,
        [Direction.LEFT]: Orientation.LEFT,
        [Direction.RIGHT]: Orientation.RIGHT
    },
    [Orientation.DOWN]: {
        [Direction.STRAIGHT]: Orientation.DOWN,
        [Direction.LEFT]: Orientation.RIGHT,
        [Direction.RIGHT]: Orientation.LEFT
    },
    [Orientation.LEFT]: {
        [Direction.STRAIGHT]: Orientation.LEFT,
        [Direction.LEFT]: Orientation.DOWN,
        [Direction.RIGHT]: Orientation.UP
    },
    [Orientation.RIGHT]: {
        [Direction.STRAIGHT]: Orientation.RIGHT,
        [Direction.LEFT]: Orientation.UP,
        [Direction.RIGHT]: Orientation.DOWN
    }
};

const MOVEMENT_MAP = {
    [Orientation.UP]: [0, -1],
    [Orientation.DOWN]: [0, 1],
    [Orientation.LEFT]: [-1, 0],
    [Orientation.RIGHT]: [1, 0]
};
const currentPosition = [0,0] as [number, number];


console.log(getPath2(heatMap));



function getPath2(heatMap: number[][]) {
    const initialPosition = [0, 0] as [number, number];
    const initialOrientation = Orientation.RIGHT;
    const initialPartialSum = 0;
    const targetPosition = [heatMap[0].length - 1, heatMap.length - 1] as [number, number];
    const distanceCache = new Map<string, number | undefined>();
    const visitedCache = new Map<string, number | undefined>();
    let minVal = 1180;

    const root = new TreeNode(heatMap[0][0], initialPosition, initialOrientation);
    let leaves = [root];
    let nextLeaves = [];
    while(leaves.length > 0) {

        for (const leaf of leaves) {

            // if (JSON.stringify(leaf.pathToRoot().reverse()).startsWith('[[0,0],[1,0],[2,0],[3,0],[4,0],[5,0],[6,0]')) {
            //     // console.log(leaf.totalHeat);
            // }

            // hay un camino mas barato para llegar a esta posicion?
            const cachedHeat = visitedCache.get(JSON.stringify([leaf.position, leaf.orientation, leaf.straights]));
            if (leaf.totalHeat > minVal || (cachedHeat && cachedHeat <= leaf.totalHeat)) {
                leaf.kill();
                continue;
            } else {
                if (leaf.position[0] === heatMap[0].length - 1 && leaf.position[1] === heatMap.length - 1) {
                    minVal = Math.min(minVal, leaf.totalHeat);
                    console.log(JSON.stringify(leaf.pathToRoot().reverse()));
                }
                visitedCache.set(JSON.stringify([leaf.position, leaf.orientation, leaf.straights]), leaf.totalHeat);
            }

            // probar izquierda
            if (leaf.straights >= 4) {
                const leftOrientation = ORIENTATION_DIRECTION_MAP[leaf.orientation][Direction.LEFT];
                const leftOffset = MOVEMENT_MAP[leftOrientation];
                const leftPosition = [leaf.position[0] + leftOffset[0], leaf.position[1] + leftOffset[1]] as [number, number];
                if (leftPosition[0] >= 0 && leftPosition[1] >= 0 && heatMap[leftPosition[1]] && heatMap[leftPosition[1]][leftPosition[0]]) {
                    const leftLeaf = new TreeNode(heatMap[leftPosition[1]][leftPosition[0]], leftPosition, leftOrientation, leaf, 1);
                    leaf.addChild(leftLeaf);
                    nextLeaves.push(leftLeaf);
                }
            }
            
            if (leaf.straights >= 4) {
            // probar derecha
                const rightOrientation = ORIENTATION_DIRECTION_MAP[leaf.orientation][Direction.RIGHT];
                const rightOffset = MOVEMENT_MAP[rightOrientation];
                const rightPosition = [leaf.position[0] + rightOffset[0], leaf.position[1] + rightOffset[1]] as [number, number];
                if (rightPosition[0] >= 0 && rightPosition[1] >= 0 && heatMap[rightPosition[1]] && heatMap[rightPosition[1]][rightPosition[0]]) {
                    const rightLeaf = new TreeNode(heatMap[rightPosition[1]][rightPosition[0]], rightPosition, rightOrientation, leaf, 1);
                    leaf.addChild(rightLeaf);
                    nextLeaves.push(rightLeaf);
                }
            }

            // probar recto
            if (leaf.straights < 10) {
                const straightOffset = MOVEMENT_MAP[leaf.orientation];
                const straightPosition = [leaf.position[0] + straightOffset[0], leaf.position[1] + straightOffset[1]] as [number, number];
                if (straightPosition[0] >= 0 && straightPosition[1] >= 0 && heatMap[straightPosition[1]] && heatMap[straightPosition[1]][straightPosition[0]]) {
                    const straightLeaf = new TreeNode(heatMap[straightPosition[1]][straightPosition[0]], straightPosition, leaf.orientation, leaf, leaf.straights + 1);
                    leaf.addChild(straightLeaf);
                    nextLeaves.push(straightLeaf);
                }
            }
            // nextLeaves = nextLeaves.filter((leaf) => leaf.alive);
            
        }
        // sort nextleaves by distance to target
        leaves = [...nextLeaves].sort((a, b) => {
            const aDistance = Math.abs(a.position[0] - targetPosition[0]) + Math.abs(a.position[1] - targetPosition[1]);
            const bDistance = Math.abs(b.position[0] - targetPosition[0]) + Math.abs(b.position[1] - targetPosition[1]);
            return bDistance - aDistance;
        }).slice(0, 260000);
        nextLeaves = [];
    }
    console.log(minVal - heatMap[0][0])
}



function getPath(heatMap: number[][]) {
    const initialPosition = [0, 0] as [number, number];
    const initialOrientation = Orientation.RIGHT;
    const initialPartialSum = 0;
    const targetPosition = [heatMap[0].length - 1, heatMap.length - 1] as [number, number];
    const distanceCache = new Map<string, number | undefined>();
    const visitedCache = new Map<string, number | undefined>();
    let minVal = 1100;

    const root = new TreeNode(heatMap[0][0], initialPosition, initialOrientation);
    let leaves = [root];
    let nextLeaves = [];
    while(leaves.length > 0) {

        for (const leaf of leaves) {
            // hay un camino mas barato para llegar a esta posicion?
            const cachedHeat = visitedCache.get(JSON.stringify([leaf.position, leaf.orientation, leaf.straights]));
            if (leaf.totalHeat > minVal || (cachedHeat && cachedHeat <= leaf.totalHeat)) {
                leaf.kill();
                continue;
            } else {
                if (leaf.position[0] === heatMap[0].length - 1 && leaf.position[1] === heatMap.length - 1) {
                    minVal = Math.min(minVal, leaf.totalHeat);
                }
                visitedCache.set(JSON.stringify([leaf.position, leaf.orientation, leaf.straights]), leaf.totalHeat);
            }

            // probar izquierda
            const leftOrientation = ORIENTATION_DIRECTION_MAP[leaf.orientation][Direction.LEFT];
            const leftOffset = MOVEMENT_MAP[leftOrientation];
            const leftPosition = [leaf.position[0] + leftOffset[0], leaf.position[1] + leftOffset[1]] as [number, number];
            if (leftPosition[0] >= 0 && leftPosition[1] >= 0 && heatMap[leftPosition[1]] && heatMap[leftPosition[1]][leftPosition[0]]) {
                const leftLeaf = new TreeNode(heatMap[leftPosition[1]][leftPosition[0]], leftPosition, leftOrientation, leaf);
                leaf.addChild(leftLeaf);
                nextLeaves.push(leftLeaf);
            }
            
            // probar derecha
            const rightOrientation = ORIENTATION_DIRECTION_MAP[leaf.orientation][Direction.RIGHT];
            const rightOffset = MOVEMENT_MAP[rightOrientation];
            const rightPosition = [leaf.position[0] + rightOffset[0], leaf.position[1] + rightOffset[1]] as [number, number];
            if (rightPosition[0] >= 0 && rightPosition[1] >= 0 && heatMap[rightPosition[1]] && heatMap[rightPosition[1]][rightPosition[0]]) {
                const rightLeaf = new TreeNode(heatMap[rightPosition[1]][rightPosition[0]], rightPosition, rightOrientation, leaf);
                leaf.addChild(rightLeaf);
                nextLeaves.push(rightLeaf);
            }

            // probar recto
            if (leaf.straights < 2) {
                const straightOffset = MOVEMENT_MAP[leaf.orientation];
                const straightPosition = [leaf.position[0] + straightOffset[0], leaf.position[1] + straightOffset[1]] as [number, number];
                if (straightPosition[0] >= 0 && straightPosition[1] >= 0 && heatMap[straightPosition[1]] && heatMap[straightPosition[1]][straightPosition[0]]) {
                    const straightLeaf = new TreeNode(heatMap[straightPosition[1]][straightPosition[0]], straightPosition, leaf.orientation, leaf, leaf.straights + 1);
                    leaf.addChild(straightLeaf);
                    nextLeaves.push(straightLeaf);
                }
            }
            // nextLeaves = nextLeaves.filter((leaf) => leaf.alive);
            
        }
        // sort nextleaves by distance to target
        leaves = [...nextLeaves].sort((a, b) => {
            const aDistance = Math.abs(a.position[0] - targetPosition[0]) + Math.abs(a.position[1] - targetPosition[1]);
            const bDistance = Math.abs(b.position[0] - targetPosition[0]) + Math.abs(b.position[1] - targetPosition[1]);
            return bDistance - aDistance;
        }).slice(0, 150000);
        nextLeaves = [];
    }
    console.log(minVal - heatMap[0][0])
}

// function getPath(heatMap: number[][], 
//     currentPosition: [number, number], 
//     partialSum: number, 
//     orientation: Orientation, 
//     straights: number, path: [number, number][]): number | undefined {
//     // console.log(currentPosition)
//     if (!heatMap[currentPosition[1]] || currentPosition[0] < 0) return undefined;
//     const currentHeat = heatMap[currentPosition[1]][currentPosition[0]]
//     if (!currentHeat) return undefined;
//     partialSum += currentHeat;
//     path.push(currentPosition);
//     if (currentPosition[0] === heatMap[0].length - 1 && currentPosition[1] === heatMap.length - 1) {
//         if (partialSum === 8) {
//             console.log(path);
//         }
//         return partialSum;
//     }
//     const possibilities: number[] = [];
//     // left
//     const nextOrientation = ORIENTATION_DIRECTION_MAP[orientation][Direction.LEFT];
//     const nextOffset = MOVEMENT_MAP[nextOrientation];
//     const nextPosition = [currentPosition[0] + nextOffset[0], currentPosition[1] + nextOffset[1]] as [number, number];
//     if (cache.has(JSON.stringify([nextPosition, nextOrientation])) && (cache.get(JSON.stringify([nextPosition, nextOrientation])) as number) <= partialSum) {
//         // console.log('cache hit');
//         path.pop();
//         return undefined;
//     } else {
//         cache.set(JSON.stringify([nextPosition, nextOrientation]), partialSum);
//         const pathLeft = getPath(heatMap, nextPosition, partialSum, nextOrientation, 0, path);
//         if (pathLeft) possibilities.push(pathLeft);
//     }
    

//     // right
//     const nextOrientation2 = ORIENTATION_DIRECTION_MAP[orientation][Direction.RIGHT];
//     const nextOffset2 = MOVEMENT_MAP[nextOrientation2];
//     const nextPosition2 = [currentPosition[0] + nextOffset2[0], currentPosition[1] + nextOffset2[1]] as [number, number];

//     if (cache.has(JSON.stringify([nextPosition2, nextOrientation2])) && (cache.get(JSON.stringify([nextPosition2, nextOrientation2])) as number) <= partialSum) {
//         // console.log('cache hit');
//         path.pop();
//         return undefined;
//     } else {
//         cache.set(JSON.stringify([nextPosition2, nextOrientation2]), partialSum);
//         const pathRight = getPath(heatMap, nextPosition2, partialSum, nextOrientation2, 0, path);
//         if (pathRight) possibilities.push(pathRight);

//     }

//     // straight
//     if (straights < 3) {
//         const nextOffset3 = MOVEMENT_MAP[orientation];
//         const nextPosition3 = [currentPosition[0] + nextOffset3[0], currentPosition[1] + nextOffset3[1]] as [number, number];
//         if (cache.has(JSON.stringify([nextPosition3, orientation])) && (cache.get(JSON.stringify([nextPosition3, orientation])) as number) <= partialSum) {
//             // console.log('cache hit');
//             path.pop();
//             return undefined;
//         } else {
            
//             cache.set(JSON.stringify([nextPosition3, orientation]), partialSum);
//             const pathStraight = getPath(heatMap, nextPosition3, partialSum, orientation, straights + 1, path);
//             if (pathStraight) possibilities.push(pathStraight);
//         }
//     }

//     return Math.min(...possibilities);

// }
