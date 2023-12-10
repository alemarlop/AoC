import { ETileType } from "./ETileType.ts";

export class Tile {
    #type: ETileType;
    #position: [number, number];
    #distanceToStart: number | undefined;

    constructor(type: string, position: [number, number]) {
        this.#type = type as ETileType;
        this.#position = position;
    }

    get type(): ETileType {
        return this.#type;
    }

    get position(): [number, number] {
        return this.#position;
    }

    get distanceToStart(): number | undefined {
        return this.#distanceToStart;
    }

    set distanceToStart(distance: number) {
        this.#distanceToStart = distance;
    }

    get x(): number {
        return this.#position[0];
    }

    get y(): number {
        return this.#position[1];
    }
}