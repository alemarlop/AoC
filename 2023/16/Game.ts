import { LightPosition, Tile } from "./Tile.ts";

export class Game {
    #tiles: Tile[][];

    constructor(tiles: string[][]) {
        this.#tiles = tiles.map((row, y) => row.map((type, x) => new Tile([x, y], type)));
    }

    getEnergizedPositions(initialLightPositions: LightPosition[]): number {
        
        const lightsPositions: Set<string> = new Set(initialLightPositions.map((lightPosition) => JSON.stringify(lightPosition.position)));
        const loopCache = new Set<string>();
        let activeLights: LightPosition[] = [...initialLightPositions];
        while (activeLights.length > 0) {
            let nextLights: LightPosition[] = [];
            activeLights.forEach((lightPosition) => {
                if (loopCache.has(JSON.stringify(lightPosition))) return;
                loopCache.add(JSON.stringify(lightPosition));

                const [x, y] = lightPosition.position;
                const tile = this.#tiles[y] ? this.#tiles[y][x] : undefined;
                if (!tile) return;
                lightsPositions.add(JSON.stringify([x, y]));
                const newDirections = tile.nextLight({direction: lightPosition.direction, position: [x, y]});
                nextLights = nextLights.concat(newDirections.map((direction) => {
                    const [v, w] = this.#tiles[y][x].movements[direction.direction](x, y);
                    const res = { position: [v, w], direction: direction.direction } as LightPosition;
                    return res;
                }));
            });
            activeLights = nextLights;
        }
        return lightsPositions.size;
    }
}