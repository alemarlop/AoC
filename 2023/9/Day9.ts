const fileName: string = Deno.args[0];
const fileLines: string[] = (await Deno.readTextFile(fileName)).split('\n');
const input = fileLines.map((line) => line.match(/\-?\d+/g)?.map(num => Number(num)));

console.log('Part 1:', [...input].reduce((sum, elem) => sum + continueSequence(elem as number[], false), 0));
console.log('Part 2:', [...input].reduce((sum, elem) => sum + continueSequence(elem as number[], true), 0));

function continueSequence(numbers: number[], previous: boolean): number {
    const expanded = expand(numbers);
    let nextNumber = 0;
    for (let i = expanded.length - 1; i >= 0; i--) {
        const current = expanded[i];
        nextNumber = current[previous ? 0 : current.length - 1] + (previous ? -1 * nextNumber : nextNumber);
        expanded[i].push(nextNumber);
    }
    return nextNumber
}

function expand(numbers: number[]): number[][] {
    const result: number[][] = [numbers];
    while (result[result.length - 1].find(num => num !== 0)) {
        const partialResult: number[] = [];
        const last = result[result.length - 1];
        for (let i = 1; i < last.length; i++) {
            partialResult.push(last[i] - last[i - 1]);
        }
        result.push(partialResult);
    }
    return result;
}