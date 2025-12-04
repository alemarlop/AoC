const fileName = Deno.args[0];
const rawFile = await Deno.readTextFile(fileName);

export class Octopus {
    private flashed = false;
    constructor(public position: [x: number, y: number], private energyLevel: number) {}

    increaseEnergy(octopusMap: Map<string, Octopus>) {
        if (this.flashed) return;
        this.energyLevel++;
        if (this.energyLevel > 9) this.flash(octopusMap);
    }

    enableFlash(): number {
        if (!this.flashed) return 0;
        this.flashed = false;
        return 1;
    }

    private flash(octopusMap: Map<string, Octopus>) {
        this.energyLevel = 0;
        this.flashed = true;
        for (const offsetX of [-1, 0, 1]) {
            for (const offsetY of [-1, 0, 1]) {
                const key = `${this.position[0] + offsetX},${this.position[1] + offsetY}`;
                if ([offsetX, offsetY].every(v => v === 0)) continue;
                if (octopusMap.has(key)) {
                    const octopus = octopusMap.get(key);
                    if (octopus) octopus.increaseEnergy(octopusMap);
                }
            }
        }
    }

}

const lines = rawFile.split('\n');
const cells = lines.map(line => line.split(''));
const octopusMap = new Map<string, Octopus>();

for (let y = 0; y < cells.length; y++) {
    for (let x = 0; x < cells[y].length; x++) {
        octopusMap.set(`${x},${y}`, new Octopus([x, y], Number(cells[y][x])));
    }
}

function runRounds(rounds: number): number {
    let flashes = 0;
    for (let i = 0; i < rounds; i++) {
        for (const octopus of octopusMap.values()) {
            octopus.increaseEnergy(octopusMap);
        }
        for (const octopus of octopusMap.values()) {
            flashes += octopus.enableFlash();
        }
    }
    return flashes;
}

function findRound(): number {
    let result = 0;
    while (true) {
        result++;
        let flashes = 0;
        for (const octopus of octopusMap.values()) {
            octopus.increaseEnergy(octopusMap);
        }
        for (const octopus of octopusMap.values()) {
            flashes += octopus.enableFlash();
            if (flashes === octopusMap.size) return result;
        }
    }
}

console.log('Part 1: ' + runRounds(100));
console.log('Part 2: ' + (findRound() + 100));