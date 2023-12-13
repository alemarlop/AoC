export class GlassMatrix {
    #glasses: string[][];
    #verticalGlasses: string[][];
    #reversedGlasses: string[][];
    #reversedVerticalGlasses: string[][];

    constructor(glasses: string[][]) {
        this.#glasses = glasses;
        this.#verticalGlasses = this.#rotateGlasses(glasses);
        this.#reversedGlasses = this.#rotateGlasses(this.#verticalGlasses);
        this.#reversedVerticalGlasses = this.#rotateGlasses(this.#reversedGlasses);
    }

    public findOther(): number {
        const [a, b] = this.countReflections();
        const count = Math.max(a ?? 0, b ?? 0);
        for (let y = 0; y < this.#glasses.length; y++) {
            const row = this.#glasses[y];
            for (let x = 0; x < row.length; x++) {
                const other = [...this.#glasses].map(r => [...r]);
                other[y][x] = other[y][x] === '#' ? '.' : '#';
                const otherMatrix = new GlassMatrix(other);
                const [c, d] = otherMatrix.countReflections();
                const otherCount = Math.max(c ?? 0, d ?? 0);
                const nw = [c, d].find(i => i !== 0 && i !== count);
                if (otherCount > 0 && !!nw) {
                    const increment = nw ? nw : otherCount;
                    return increment;
                }
            }
        }
        return 0;
    }

    public countReflections(): [number, number] {
        const result: number[] = [];
        const horizontal = this.#countReflections(this.#glasses)
        const vertical = this.#countReflections(this.#verticalGlasses);
        let reverseHorizontal = this.#countReflections(this.#reversedGlasses);
        let reverseVertical = this.#countReflections(this.#reversedVerticalGlasses);
        reverseHorizontal = reverseHorizontal !== 0 ? this.#reversedGlasses.length - reverseHorizontal: 0;
        reverseVertical = reverseVertical !== 0 ? this.#reversedVerticalGlasses.length - reverseVertical: 0;
        if (horizontal > 0) {
            result.push(horizontal * 100)
        }
        if (reverseHorizontal > 0) {
            result.push(reverseHorizontal * 100);
        } 
        if (vertical > 0) {
            result.push(vertical);
        }
        if (reverseVertical > 0) {
            result.push(reverseVertical);
        }
        return result as [number, number];
    }

    #countReflections(matrix: string[][]): number {
        let reflections = 0;
        for (let i = 0; i < matrix.length; i++) {
            const oppositeIndex = matrix.length - 1 - reflections;
            const currentRow = matrix[i];
            const oppositeRow = matrix[oppositeIndex];
            if (currentRow.join('') === oppositeRow.join('')) {
                reflections++;
            } else {
                reflections = 0;
                const newOppositeIndex = matrix.length - 1 - reflections;
                reflections = currentRow.join('') === matrix[newOppositeIndex].join('') ? 1 : 0;
            }
            if (Math.abs(oppositeIndex - i) === 1 && reflections > 0 && reflections === matrix.length - i - 1) return i + 1;
        }
        return 0;
    }

    #rotateGlasses(glasses: string[][]): string[][] {
        const rotatedGlasses: string[][] = [];
        for (let i = 0; i < glasses[0].length; i++) {
            rotatedGlasses.push([]);
            for (let j = glasses.length - 1; j >= 0; j--) {
                rotatedGlasses[i].push(glasses[j][i]);
            }
        }
        return rotatedGlasses;
    }
}