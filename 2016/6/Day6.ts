const fileName: string = Deno.args[0];
const input: string[] = (await Deno.readTextFile(fileName)).split('\n');

type Comparator = (a: number, b: number) => number

console.log(`Part 1: ${getCorrectedVersion(input, Math.max)}`);
console.log(`Part 2: ${getCorrectedVersion(input, Math.min)}`);

function getCorrectedVersion(words: string[], f: Comparator): string {
    const occurencesMap: Map<string, number>[] = [];
    words.forEach(word => {
        word.split('').forEach((character, index) => {
            if (!occurencesMap[index]) {
                const newMap = new Map();
                newMap.set(character, 1);
                occurencesMap[index] = newMap;
            } else {
                const existingMap = occurencesMap[index];
                existingMap.set(character, existingMap.has(character) ? existingMap.get(character) as number + 1: 1);
            }
        })
    });
    let result = '';
    occurencesMap.forEach((_, index) => {
        const mapForIndex = occurencesMap[index];
        result += Array.from(mapForIndex.keys()).reduce((curr, acc) => 
            f((mapForIndex.get(acc) as number), (mapForIndex.get(curr) as number)) == mapForIndex.get(acc) as number ? acc: curr); 
    });

    return result;
}