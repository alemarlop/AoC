export class Conjunction {
    #memory: Map<string, 'hi' | 'lo'>;
    #tag: string;
    #inputTag: string[];
    #outputTag: string[];

    constructor(tag: string, inputTag: string[], outputTag: string[]) {
        this.#tag = tag;
        this.#inputTag = inputTag;
        this.#outputTag = outputTag;
        this.#memory = new Map();
        inputTag.forEach(tag => this.#memory.set(tag, 'lo'));
    }

    public get inputTags(): string[] {
        return this.#inputTag;
    }
    public get outputTags(): string[] {
        return this.#outputTag;
    }

    public get tag(): string {
        return this.#tag;
    }

    public print(): void {
        console.log(`Conjunction ${this.#tag} -> ${this.#outputTag} (${JSON.stringify([...this.#memory.keys()])})`);
    }

    public run(input: 'hi' | 'lo', from: string): [string, 'hi' | 'lo'][] | undefined {
        this.#memory.set(from, input);
        if (this.#inputTag.every(t => this.#memory.get(t) === 'hi')) {
            const res = this.#outputTag.map(tag => [tag, 'lo'] as [string, 'hi' | 'lo']);
            // res.forEach(([tag, val]) => console.log(`C ${this.#tag} -> ${val} -> ${tag}`));
            return res;
        }
        const res = this.#outputTag.map(tag => [tag, 'hi'] as [string, 'hi' | 'lo']);
        // res.forEach(([tag, val]) => console.log(`C ${this.#tag} -> ${val} -> ${tag}`));
        return res;

    }
}