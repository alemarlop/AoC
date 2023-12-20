export class FlipFlop {
    #inputTag: string;
    #outputTag: string[];
    #state: 'on' | 'off';

    constructor(inputTag: string, outputTag: string[]) {
        this.#inputTag = inputTag;
        this.#outputTag = outputTag;
        this.#state = 'off';
    }

    public get inputTag(): string {
        return this.#inputTag;
    }

    public get state(): 'on' | 'off' {
        return this.#state;
    }

    public print(): void {
        console.log(`FlipFlop ${this.#inputTag} -> ${this.#state} -> ${this.#outputTag}`);
    }

    public run(input: 'hi' | 'lo'): [string, 'hi' | 'lo'][] | undefined {
        if (input === 'hi') return;
        if (this.#state === 'off') {
            this.#state = 'on';
            const res = this.#outputTag.map(tag => [tag, 'hi'] as [string, 'hi' | 'lo']);
            // res.forEach(([tag, val]) => console.log(`FF ${this.#inputTag} -> ${val} -> ${tag}`));
            return res;
        }
        this.#state = 'off';
        const res = this.#outputTag.map(tag => [tag, 'lo'] as [string, 'hi' | 'lo']);
        // res.forEach(([tag, val]) => console.log(`FF ${this.#inputTag} -> ${val} -> ${tag}`));
        return res;
    }
}