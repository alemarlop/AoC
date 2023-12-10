import { ETileType } from "./ETileType.ts";
import { Tile } from "./Tile.ts";

enum EOrientation {
    NORTH,
    EAST,
    SOUTH,
    WEST,
}

const MOVE_MAP: { [key in EOrientation]: (tile: Tile) => [number, number] } = {
    [EOrientation.NORTH]: (tile: Tile) => [tile.x, tile.y - 1],
    [EOrientation.EAST]: (tile: Tile) => [tile.x + 1, tile.y],
    [EOrientation.SOUTH]: (tile: Tile) => [tile.x, tile.y + 1],
    [EOrientation.WEST]: (tile: Tile) => [tile.x - 1, tile.y],
};

const TILE_TYPE_ORIENTATION_CHANGE: any = {
    [ETileType.EAST_NORTH]: { [ETileType.WEST_SOUTH]: 1 },
    [ETileType.EAST_SOUTH]: { [ETileType.WEST_NORTH]: 1 },
    [ETileType.WEST_NORTH]: { [ETileType.EAST_SOUTH]: 1 },
    [ETileType.WEST_SOUTH]: { [ETileType.EAST_NORTH]: 1 },
}

const ORIENTATION_MAP: { [key in EOrientation]: { [key: string]: EOrientation | undefined } } = {
    [EOrientation.NORTH]: {
        [ETileType.VERTICAL]: EOrientation.NORTH,
        [ETileType.WEST_SOUTH]: EOrientation.WEST,
        [ETileType.EAST_SOUTH]: EOrientation.EAST,
    },
    [EOrientation.EAST]: {
        [ETileType.HORIZONTAL]: EOrientation.EAST,
        [ETileType.WEST_NORTH]: EOrientation.NORTH,
        [ETileType.WEST_SOUTH]: EOrientation.SOUTH,
    },
    [EOrientation.SOUTH]: {
        [ETileType.VERTICAL]: EOrientation.SOUTH,
        [ETileType.WEST_NORTH]: EOrientation.WEST,
        [ETileType.EAST_NORTH]: EOrientation.EAST,
    },
    [EOrientation.WEST]: {
        [ETileType.HORIZONTAL]: EOrientation.WEST,
        [ETileType.EAST_NORTH]: EOrientation.NORTH,
        [ETileType.EAST_SOUTH]: EOrientation.SOUTH,
    }
};

export class TileMap {
    #tiles: Tile[][];
    #startingPoint: Tile | undefined;

    constructor(tiles: string[][]) {
        this.#tiles = tiles.map((line, y) => line.map((type, x) => {
            const newTile = new Tile(type, [x, y]);
            if (type === ETileType.STARTING_POINT) {
                this.#startingPoint = newTile;
            }
            return newTile;
        }));
    }

    public getMaxDistance(): number {
        const tiles = this.getLoop(EOrientation.SOUTH) || this.getLoop(EOrientation.NORTH) || this.getLoop(EOrientation.EAST) || this.getLoop(EOrientation.WEST);
        return ((tiles as Tile[])[(tiles as Tile[]).length - 1].distanceToStart as number)/2;
    }

    public getArea(): number {
        let count = 0;
        const loop = this.getLoop(EOrientation.SOUTH) || this.getLoop(EOrientation.NORTH) || this.getLoop(EOrientation.EAST) || this.getLoop(EOrientation.WEST);
        const maxY = Math.max(...(loop as Tile[]).map((tile) => tile.y));
        const minY = Math.min(...(loop as Tile[]).map((tile) => tile.y));
        const maxX = Math.max(...(loop as Tile[]).map((tile) => tile.x));
        const minX = Math.min(...(loop as Tile[]).map((tile) => tile.x));
        for (let y = minY + 1; y < maxY; y++) {
            for (let x = minX + 1; x < maxX; x++) {
                if (loop?.find((tile) => tile.x === x && tile.y === y)) {
                    continue;
                }
                const verticalWalls = (loop as Tile[]).filter((tile) => tile.x === x && tile.y < y);
                const horizontalWalls = (loop as Tile[]).filter((tile) => tile.y === y && tile.x < x);
                const countVerticalWalls = verticalWalls.filter((tile) => tile.type === ETileType.HORIZONTAL).length;
                const countHorizontalWalls = horizontalWalls.filter((tile) => tile.type === ETileType.VERTICAL).length;
                const closestHorizontalWallsSorted = horizontalWalls
                    .filter((tile) => tile.type !== ETileType.HORIZONTAL && tile.type !== ETileType.VERTICAL)
                    .sort((a, b) => Math.abs(a.x - x) - Math.abs(b.x - x));

                const countCornersWithoutChangeDirectionHorizontal = this.countCornersWithoutChangeDirection(closestHorizontalWallsSorted);

                const closestVerticalWallsSorted = verticalWalls
                    .filter((tile) => tile.type !== ETileType.HORIZONTAL && tile.type !== ETileType.VERTICAL)
                    .sort((a, b) => Math.abs(a.y - y) - Math.abs(b.y - y));

                const countCornersWithoutChangeDirectionVertical = this.countCornersWithoutChangeDirection(closestVerticalWallsSorted);

                if ((countCornersWithoutChangeDirectionHorizontal + countHorizontalWalls)%2 === 0 || 
                    (countCornersWithoutChangeDirectionVertical + countVerticalWalls)%2 === 0) {
                    continue;
                }
                count++;
            }
        }
        return count;
    }

    private countCornersWithoutChangeDirection(tiles: Tile[]): number { 
        let count = 0;
        for (let i = 0; i < tiles.length - 1; i+=2) {
            const currentTile = tiles[i];
            const nextTile = tiles[i + 1];
            const val = TILE_TYPE_ORIENTATION_CHANGE[currentTile.type]?.[nextTile.type] ?? 0;
            count += val;
        }
        return count;
    }

    public getLoop(orientation: EOrientation): Tile[] | undefined  {
        let currentTile = this.#startingPoint;
        let distanceToStart = 0;
        (currentTile as Tile).distanceToStart = distanceToStart;
        const loop: Tile[] = [currentTile as Tile];
        while (currentTile) {
            currentTile = this.getNextTile(currentTile, orientation);
            distanceToStart++;
            (currentTile as Tile).distanceToStart = distanceToStart;
            orientation = ORIENTATION_MAP[orientation][currentTile?.type as ETileType] as EOrientation;
            if (currentTile === this.#startingPoint) {
                loop.push(currentTile as Tile);
                break;
            }
            if (orientation === undefined) {
                return undefined;
            }
            loop.push(currentTile as Tile);
        }
        return loop;
    }

    private getNextTile(tile: Tile, orientation: EOrientation): Tile | undefined {
        const nextPosition = MOVE_MAP[orientation](tile);
        if (nextPosition) {
            return this.#tiles[nextPosition[1]][nextPosition[0]];
        }
        return undefined;
    }
}