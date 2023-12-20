import { Conjunction } from "./Conjunction.ts";
import { FlipFlop } from "./FlipFlop.ts";

const fileName: string = Deno.args[0];
const fileConent: string = await Deno.readTextFile(fileName);

const lines: string[] = fileConent.split('\n');
const map: Map<string, string[]> = new Map();
lines.forEach(line => {
    const [input, output] = line.split(' -> ');
    map.set(input, output.split(', '));
});

const broadcaster = map.get('broadcaster');

function parseFlipFlop(): FlipFlop[] {
    const flipLops: FlipFlop[] = [...map.keys()].filter(key => key.startsWith('%'))
        .map(key => new FlipFlop(key.substring(1), map.get(key) as string[]));
    return flipLops;
}


function parseConjunction(): Conjunction[] {
    const conjunctions: Conjunction[] = [];
    [...map.keys()].filter(key => key.startsWith('&')).forEach(key => {
        const input = [...map.keys()].filter(key1 => map.get(key1)?.includes(key.substring(1))).map((k: string) => k.substring(1));
        conjunctions.push(new Conjunction(key.substring(1), input, map.get(key) as string[]));
    });
    return conjunctions;
}

function press(tags: [string, 'hi' | 'lo', string][], flipFlops: FlipFlop[], conjunctions: Conjunction[], stopAt?: string, ): [number, number] {
    let [hi, lo] = [0, tags.length + 1];
    while (tags.length > 0) {
        const next: [string, 'hi' | 'lo', string][] = [];
        tags.forEach(tag => {
            const flipLop = flipFlops.find(flipLop => flipLop.inputTag === tag[0]);
            if (flipLop) {
                const result = flipLop.run(tag[1] as 'hi' | 'lo');
                if (result) {
                    result.forEach(tag => {
                        if (tag[1] === 'hi') hi++;
                        else lo++;
                        next.push([tag[0], tag[1], flipLop.inputTag])
                    });
                }

            }
            const conjunction = conjunctions.find(conjunction => conjunction.tag === tag[0]);
            if (conjunction) {
                const result = conjunction.run(tag[1], tag[2]);
                if (result) {
                    result.forEach(tag => {
                        if (tag[1] === 'hi') hi++;
                        else lo++;
                        next.push([tag[0], tag[1], conjunction.tag])
                    });
                }
            }
        });
        tags = [...next];
        const t = tags.find(tag => tag[2] === stopAt && tag[1] === 'hi')
        if (stopAt && t) {
            return [-1, -1];
        }
    }
    return [hi, lo];
}

function pressNTimes(n: number): number {
    let [hi, lo] = [0, 0];
    const flipFlops = parseFlipFlop();
    const conjunctions = parseConjunction();
    for (let i = 0; i < n; i++) {
        const result = press((broadcaster as any[]).map(tag => [tag, 'lo', 'broadcaster']), flipFlops, conjunctions);
        hi += result[0];
        lo += result[1];
    }
    return hi * lo;
}

function singleRx(): number {
    const values: number[] = [];
    // const flipFlops = parseFlipFlop();
    const conjunctions = parseConjunction();
    const conj = conjunctions.find(conjunction => conjunction.outputTags.includes('rx'));
    const toCalculate = conjunctions.filter(conjunction => conjunction.outputTags.includes((conj as Conjunction).tag))
        .map(conjunction => conjunction.tag);
    
    toCalculate.forEach(tag => {
        let count = 0;
        const newFlipFlops = parseFlipFlop();
        const newConjunctions = parseConjunction();
        let [hi, lo] = [0, 0];
        while (hi >= 0) {
            [hi, lo] = press((broadcaster as any[]).map(tag => [tag, 'lo', 'broadcaster']), newFlipFlops, newConjunctions, tag);
            count++;
        }
        values.push(count);
    });

    // calcuale mcm of values
    let mcm = values[0];
    for (let i = 1; i < values.length; i++) {
        mcm = (mcm * values[i]) / gcd(mcm, values[i]);
    }
    return mcm;
}

function gcd(a: number, b: number): number {
    if (b === 0) return a;
    return gcd(b, a % b);
}

console.log(`Part 1: ${pressNTimes(1000)}`);
console.log(`Part 2: ${singleRx()}`);