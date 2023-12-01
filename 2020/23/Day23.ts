const input_size: number = 50;
const turns: number = 500;

const input: string = await Deno.readTextFile("./test.txt");

let cups: number[] = input.split("").map((x) => Number(x));
cups = initCupsMap(cups);

console.log(cups);
console.log(initCupsMap(cups))

function initCupsMap(cups: number[]): number[] {
    const response = []
    for (let i = 0; i < cups.length; i++) {
        const current: number = cups[i];
        const next: number = i == cups.length - 1 ? cups[0] : cups[i+1];
        response[current] = next;
    }

    return response;
}

function playRound(cups: number[], selected: number): number {
    const cup1 = cups[selected];
    const cup2 = cups[cup1];
    const cup3 = cups[cup2];

    let destination = selected - 1
    while ([cup1, cup2, cup3].includes(destination) || destination < 1) {
        destination--;
        if (destination < 1) destination = 9;
    }

    const afterSel = cups[destination];
    
}
