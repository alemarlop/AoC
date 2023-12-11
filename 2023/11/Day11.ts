import { Sky } from "./Sky.ts";

const fileName: string = Deno.args[0];
const fileContent = await Deno.readTextFile(fileName);

const rawMap = fileContent.split("\n").map(row => row.split(""));
const sky = new Sky(rawMap);

console.log('Part 1:', [...sky.getGalaxyDistances(1).values()].reduce((a, b) => a + b, 0));
console.log('Part 2:', [...sky.getGalaxyDistances(1000000).values()].reduce((a, b) => a + b, 0));