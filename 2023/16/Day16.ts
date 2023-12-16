import { Game } from "./Game.ts";
import { Direction } from "./Tile.ts";

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);
const rawMatrix: string[][] = fileContent.split('\n').map((row) => row.split(''));

const game = new Game(rawMatrix);

console.log('Part 1:', game.getEnergizedPositions([{ position: [0, 0], direction: Direction.RIGHT }]));
console.log('Part 2:', part2(game));

function part2(game: Game) {
    // corners
    const results: number[] = [];
    results.push(game.getEnergizedPositions([{ position: [0, 0], direction: Direction.RIGHT }]));
    results.push(game.getEnergizedPositions([{ position: [0, 0], direction: Direction.DOWN }]));
    results.push(game.getEnergizedPositions([{ position: [rawMatrix[0].length - 1, 0], direction: Direction.LEFT }]));
    results.push(game.getEnergizedPositions([{ position: [rawMatrix[0].length - 1, 0], direction: Direction.DOWN }]));
    results.push(game.getEnergizedPositions([{ position: [0, rawMatrix.length - 1], direction: Direction.UP }]));
    results.push(game.getEnergizedPositions([{ position: [0, rawMatrix.length - 1], direction: Direction.RIGHT }]));
    results.push(game.getEnergizedPositions([{ position: [rawMatrix[0].length - 1, rawMatrix.length - 1], direction: Direction.LEFT }]));
    results.push(game.getEnergizedPositions([{ position: [rawMatrix[0].length - 1, rawMatrix.length - 1], direction: Direction.UP }]));

    // borders
    for (let i = 0; i < rawMatrix[0].length - 1; i++) {
        results.push(game.getEnergizedPositions([{ position: [i, 0], direction: Direction.DOWN }]));
        results.push(game.getEnergizedPositions([{ position: [i, rawMatrix.length - 1], direction: Direction.UP }]));
    }

    for (let i = 0; i < rawMatrix.length - 1; i++) {
        results.push(game.getEnergizedPositions([{ position: [0, i], direction: Direction.RIGHT }]));
        results.push(game.getEnergizedPositions([{ position: [rawMatrix[0].length - 1, i], direction: Direction.LEFT }]));
    }
    return Math.max(...results);
}