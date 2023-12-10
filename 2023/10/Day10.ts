import { TileMap } from "./TileMap.ts";

const fileName: string = Deno.args[0];
const content = await Deno.readTextFile(fileName);
const map = content.split("\n").map((line) => line.split(''));

const tileMap: TileMap = new TileMap(map);
console.log('Part 1:', tileMap.getMaxDistance());
console.log('Part 2:', tileMap.getArea());
