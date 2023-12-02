type Color = 'red' | 'green' | 'blue';

interface Game {
    id: number,
    sets: GameSet[]
}

type GameSet = {
    [color in Color]: number
}

const MAX_BLUE_CUBES = 14;
const MAX_RED_CUBES = 12;
const MAX_GREEN_CUBES = 13;

const fileName: string = Deno.args[0];
const games: string[] = (await Deno.readTextFile(fileName)).split('\n');

console.log('Part 1:', getPossibleGamesSum(parseGames(games)));
console.log('Part 2:', getMinCubesSum(parseGames(games)));

function getPossibleGamesSum(games: Game[]): number {
    return games.reduce((sum, game) => {
        const findExpression = (set: GameSet) => set['red'] > MAX_RED_CUBES || set['green'] > MAX_GREEN_CUBES || set['blue'] > MAX_BLUE_CUBES;
        if (game.sets.find(findExpression)) return sum;
        return sum + game.id;
    }, 0);
}

function getMinCubesSum(games: Game[]): number {
    return games.reduce((result, game) => {
        const reds: number[] = game.sets.map(set => set['red'])
        const green: number[] = game.sets.map(set => set['green'])
        const blue: number[] = game.sets.map(set => set['blue'])
        return result += Math.max(...reds) * Math.max(...green) * Math.max(...blue);
    }, 0);
}

function parseGames(games: string[]): Game[] {
    return games.map(game => {
        const [id, rawGame] = game.split(':');
        const sets = rawGame.split(';');
        const gameSets = sets.map(set => {
            const cubes = set.split(',').map(cube => cube.trim());
            return cubes.reduce((cubeCount, currentCube) => {
                const [count, color] = currentCube.split(' ');
                cubeCount[color as Color] += Number(count);
                return cubeCount;
            }, { red: 0, green: 0, blue: 0 } as GameSet);
        });
        const parsedGame: Game = {
            id: Number((id.match(/\d+/) as RegExpMatchArray)[0]),
            sets: gameSets
        }

        return parsedGame;
    });
}