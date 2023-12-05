import { Range } from "./Range.ts";

export class Step {
    #name: string;
    #ranges: Range[];

    constructor(rawStep: string) {
        this.#ranges = [];
        const [name, rawRanges] = rawStep.split(':');
        this.#name = name;
        const lines: string[] = rawRanges.split('\n');
        lines.forEach(line => {
            if (line.length > 0) {
                const [destination, source, length] = line.match(/\d+/g)?.map(match => Number(match)) as [number, number, number];
                this.#ranges.push(new Range(destination, source, length));
            }
        });
        this.sortAscMap();
    }

    public map(value: number): number {
        for (const range of this.#ranges) {
            const candidate = range.transform(value);
            if (value !== candidate) return candidate;
        }
        return value;
    }

    // public mapMany(inputRanges: [number, number][]): [number, number][] {
    //     const response = [];
    //     inputRanges.forEach(inputRange => {
    //         this.#ranges.forEach(range => {
    //             const from = range.destination;
    //             const to = from + range.length;

    //         });
    //     });
    // }

    private sortAscMap() {
        this.#ranges.sort((r1, r2) => r1.destination - r2.destination);
    }
}