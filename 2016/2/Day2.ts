const fileName: string = Deno.args['0'];
const input = await Deno.readTextFile(fileName);
const movements = {
    '1': {'U': '1', 'D': '4', 'L': '1', 'R': '2'},
    '2': {'U': '2', 'D': '5', 'L': '1', 'R': '3'},
    '3': {'U': '3', 'D': '6', 'L': '2', 'R': '3'},
    '4': {'U': '1', 'D': '7', 'L': '4', 'R': '5'},
    '5': {'U': '2', 'D': '8', 'L': '4', 'R': '6'},
    '6': {'U': '3', 'D': '9', 'L': '5', 'R': '6'},
    '7': {'U': '4', 'D': '7', 'L': '7', 'R': '8'},
    '8': {'U': '5', 'D': '8', 'L': '7', 'R': '9'},
    '9': {'U': '6', 'D': '9', 'L': '8', 'R': '9'}
} as Record<string, Record<string, string>>;

const fancyMovements = {
    '1': {'U': '1', 'D': '3', 'L': '1', 'R': '1'},
    '2': {'U': '2', 'D': '6', 'L': '2', 'R': '3'},
    '3': {'U': '1', 'D': '7', 'L': '2', 'R': '4'},
    '4': {'U': '4', 'D': '8', 'L': '3', 'R': '4'},
    '5': {'U': '5', 'D': '5', 'L': '5', 'R': '6'},
    '6': {'U': '2', 'D': 'A', 'L': '5', 'R': '7'},
    '7': {'U': '3', 'D': 'B', 'L': '6', 'R': '8'},
    '8': {'U': '4', 'D': 'C', 'L': '7', 'R': '9'},
    '9': {'U': '9', 'D': '9', 'L': '8', 'R': '9'},
    'A': {'U': '6', 'D': 'A', 'L': 'A', 'R': 'B'},
    'B': {'U': '7', 'D': 'D', 'L': 'A', 'R': 'C'},
    'C': {'U': '8', 'D': 'C', 'L': 'B', 'R': 'C'},
    'D': {'U': 'B', 'D': 'D', 'L': 'D', 'R': 'D'}
} as Record<string, Record<string, string>>;

const lines = input.split('\r\n');

let start = performance.now();
const code: string = playGame('5', lines, movements);
let end = performance.now();
console.log(`Part 1: ${code} \t| ${end - start}ms`);
start = performance.now();
const fancyCode: string = playGame('5', lines, fancyMovements);
end = performance.now();
console.log(`Part 2: ${fancyCode} \t| ${end - start}ms`);

function playGame(initialNumber: string, lines: string[], movementRecord: Record<string, Record<string, string>>): string {
    let number = initialNumber;
    return lines.reduce((acc, curr) => {
        curr.split('').forEach(move => number = movementRecord[number][move]);
        acc += number;
        return acc;
    }, '');
}
