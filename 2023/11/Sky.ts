export class Sky {
    #galaxies: Map<number, [number, number]>;
    #emptyHorizontals: number[];
    #emptyVerticals: number[];

    constructor(rawMap: string[][]) {
        let count = 0;
        this.#galaxies = new Map();
        this.#emptyHorizontals = [];
        this.#emptyVerticals = [];

        rawMap.forEach((row, y) => {
            if (row.every(cell => cell === ".")) {
                this.#emptyHorizontals.push(y);
            }
            row.forEach((cell, x) => {
                if (cell === "#") {
                    this.#galaxies.set(++count, [x, y]);
                }
            });
        });
        for (let x = 0; x < rawMap[0].length; x++) {
            let empty = true;
            for (let y = 0; y < rawMap.length; y++) {
                if (rawMap[y][x] !== ".") {
                    empty = false;
                    break;
                }
            }
            if (empty) this.#emptyVerticals.push(x)
        }
    }

    public getGalaxyDistances(expansionFactor: number): Map<string, number> {
        const distances = new Map<string, number>();
        this.#galaxies.forEach((coords, galaxy) => {
            this.#galaxies.forEach((otherCoords, otherGalaxy) => {
                if (galaxy !== otherGalaxy) {
                    if (distances.has(`${otherGalaxy},${galaxy}`)) {
                        return;
                    };
                    const distance = Math.abs(coords[0] - otherCoords[0]) + Math.abs(coords[1] - otherCoords[1]);
                    const countVerticals = this.#emptyVerticals.filter(x => coords[0] <= x && x <= otherCoords[0] || otherCoords[0] <= x && x <= coords[0]);
                    const countHorizontals = this.#emptyHorizontals.filter(y => coords[1] <= y && y <= otherCoords[1] || otherCoords[1] <= y && y <= coords[1]);
                    distances.set(`${galaxy},${otherGalaxy}`, distance + countHorizontals.length * (expansionFactor - 1) + countVerticals.length * (expansionFactor - 1));
                }
            });
        });
        return distances;
    }

}