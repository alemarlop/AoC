export class Tile {
    #movements = {
        [Direction.UP]: (x: number, y: number) => [x, y - 1],
        [Direction.DOWN]: (x: number, y: number) => [x, y + 1],
        [Direction.LEFT]: (x: number, y: number) => [x - 1, y],
        [Direction.RIGHT]: (x: number, y: number) => [x + 1, y]
    }

    #direction_movements: any = {
        [TileType.EMPTY_SPACE]: {
            [Direction.UP]: [Direction.UP],
            [Direction.DOWN]: [Direction.DOWN],
            [Direction.LEFT]: [Direction.LEFT],
            [Direction.RIGHT]: [Direction.RIGHT]
        },
        [TileType.HORIZONTAL_PIPE]: {
            [Direction.UP]: [Direction.LEFT, Direction.RIGHT],
            [Direction.DOWN]: [Direction.LEFT, Direction.RIGHT],
        },
        [TileType.VERTICAL_PIPE]: {
            [Direction.LEFT]: [Direction.UP, Direction.DOWN],
            [Direction.RIGHT]: [Direction.UP, Direction.DOWN],
        },
        [TileType.MIRROR_RIGHT]: {
            [Direction.UP]: [Direction.LEFT],
            [Direction.DOWN]: [Direction.RIGHT],
            [Direction.LEFT]: [Direction.UP],
            [Direction.RIGHT]: [Direction.DOWN]
        },
        [TileType.MIRROR_LEFT]: {
            [Direction.UP]: [Direction.RIGHT],
            [Direction.DOWN]: [Direction.LEFT],
            [Direction.LEFT]: [Direction.DOWN],
            [Direction.RIGHT]: [Direction.UP]
        }
    }

    
    #position: [number, number];
    #type: TileType;

    constructor(position: [number, number], type: string) {
        this.#position = position;
        this.#type = type as TileType;
    }

    get position(): [number, number] {
        return this.#position;
    }

    get type(): TileType {
        return this.#type;
    }
    
    get movements(): any {
        return this.#movements;
    }

    nextLight(lightPosition: LightPosition): LightPosition[] {
        const direction: Direction = lightPosition.direction;
        const [x, y] = lightPosition.position;
        const nextTileType = this.#direction_movements[this.#type][direction] as Direction[] ?? [direction];
        return nextTileType.map((direction) => ({ position: [x, y], direction } as LightPosition));
    }
}

export enum TileType {
    EMPTY_SPACE = '.',
    HORIZONTAL_PIPE = '-',
    VERTICAL_PIPE = '|',
    MIRROR_LEFT = '/',
    MIRROR_RIGHT = '\\'
}

export enum Direction {
    UP = 'UP',
    DOWN = 'DOWN',
    LEFT = 'LEFT',
    RIGHT = 'RIGHT'
}

export interface LightPosition {
    position: [number, number];
    direction: Direction;
}