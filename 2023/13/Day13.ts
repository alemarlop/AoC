import { GlassMatrix } from "./GlassMatrix.ts";

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);
const matrix: string[][][] = fileContent.split("\n\n").map(matrix => matrix.split('\n').map((row) => row.split("")));

const glassMatrixList: GlassMatrix[] = matrix.map(m => new GlassMatrix(m));
console.log(`Part 1: ${glassMatrixList.reduce((a, b) => a + Math.max(...b.countReflections()), 0)}`);
console.log(`Part 2: ${glassMatrixList.reduce((a, b) => a + b.findOther(), 0)}`);
// const ma = new GlassMatrix(matrix[0])
// console.log(ma.countReflections());
// // console.log(ma.print());
