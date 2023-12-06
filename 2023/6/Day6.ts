interface Race {
    time: number;
    distance: number;
}

const fileName: string = Deno.args[0];
const fileText: string = await Deno.readTextFile(fileName);

console.log('Part 1:', parseInput(fileText).reduce((count,race) => getWinningPlays(race) * count, 1));
console.log('Part 2:', getWinningPlays(parseInputPart2(fileText)));

function getWinningPlays(race: Race): number {
    const a = -1;
    const b = race.time;
    const c = -race.distance;
    const discriminante = (b * b) - (4 * a * c);
    const raiz1 = (-b - Math.sqrt(discriminante)) / (2 * a);
    const raiz2 = (-b + Math.sqrt(discriminante)) / (2 * a);
    const difference = Math.ceil(Math.max(raiz1, raiz2)) - Math.floor(Math.min(raiz1, raiz2));
    return difference - 1;
}

function parseInput(fileText: string): Race[] {
    const result: Race[] = [];
    const [times, distance]: [number[], number[]] = 
        fileText.split('\n').map((line: string) => line.match(/(\d+)/g)?.map(elem => Number(elem))) as [number[], number[]];
    for (let i = 0; i < times.length; i++) {
        result.push({time: times[i], distance: distance[i]});
    }
    return result;
}

function parseInputPart2(fileText: string): Race {
    const [times, distance]: [number[], number[]] = 
        fileText.split('\n').map((line: string) => line.match(/(\d+)/g)?.map(elem => Number(elem))) as [number[], number[]];

    return {time: Number(times.join('')), distance: Number(distance.join(''))};   
}