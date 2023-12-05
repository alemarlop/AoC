import { Step } from "./Step.ts";

interface Game {
    steps: Step[],
    seeds: number[]
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);

console.log('Part 1:', Math.min(...getSeedsLocation(parseInput(fileContent))));
console.log('Part 1:', getCrazyLocations(parseInput(fileContent)));

function getCrazyLocations(game: Game): number {
    let min: number | undefined;
    const processed: [number, number][] = [];
    let cached = 0;
    for (let i = 0; i < game.seeds.length; i+=2) {
        const seed = game.seeds[i];
        const range = seed + game.seeds[i+1];
        for (let j = seed; j < range; j++) {
            let value = j;
            if (processed.find(pr => pr[0] <= value && value < pr[1])) {
                cached++;
                continue;
            }
            game.steps.forEach(step => {
                value = step.map(value);
            });
            if (!min || value < min) {
                min = value;
            }
        }
        console.log('processed one range!', cached)
        processed.push([seed, range]);
    }
    return min as number;
}

function getSeedsLocation(game: Game): number[] {
    const locations = game.seeds.map(seed => {
        let value = seed;
        game.steps.forEach(step => {
            value = step.map(value);
        });
        return value;
    });
    return locations;
}

function parseInput(rawContent: string): Game {
    const [rawSeeds, ...rawSteps] = rawContent.split('\n\n');
    const seeds = rawSeeds.match(/\d+/g)?.map(seed => Number(seed)) as number[];
    return {
        seeds,
        steps: rawSteps.map(step => new Step(step))
    }
}