export class Range {
    #destination: number;
    #source: number;
    #length: number;

    constructor(destination: number, source: number, length: number) {
        this.#destination = destination;
        this.#source = source;
        this.#length = length
    }

    private isInRange(value: number): boolean {
        return value >= this.#source && value < (this.#source + this.#length);
    }

    public transform(value: number): number {
        if (!this.isInRange(value)) return value;
        return (value - this.#source) + this.#destination;
    }

    public get destination() {
        return this.#destination;
    }
    
    public get source() {
        return this.#source;
    }

    public get length() {
        return this.#length;
    }
}