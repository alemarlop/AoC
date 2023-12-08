interface Instructions {
    operations: string[];
    map: Map<string, [string, string]>;
}

const fileName: string = Deno.args[0];
const fileInput = await Deno.readTextFile(fileName);

console.log(countSteps(parseInput(fileInput), 'AAA'));
console.log(countSteps1(parseInput(fileInput)))

function countSteps(instructions: Instructions, startingElement: string): number {
    const { operations, map } = instructions;
    let pointer = 0;
    let currentElement = startingElement;
    const targetElement = 'ZZZ';
    const insturctionsLength: number = operations.length;
    while (!currentElement.endsWith('Z')) {
        const [left, right] = map.get(currentElement) as [string, string];
        if (operations[pointer%insturctionsLength] === 'L') {
            currentElement = left;
        } else {
            currentElement = right;
        }
        pointer++;
    }
    return pointer;
}

function countSteps1(instructions: Instructions): number {
    const { operations, map } = instructions;
    const currentElements = [...map.keys()].filter((key) => key.endsWith('A')) as string[];
    const results = currentElements.map((element) => {
        const res = countSteps(instructions, element);
        return res;
    });
    // minimum common multiple


    return minimumCommonMultiple(results);
}

function minimumCommonMultiple(numbers: number[]): number {
    const max = Math.max(...numbers);
    let result = max;
    while (true) {
        if (numbers.every((number) => result % number === 0)) {
            return result;
        }
        result += max;
    }
}

// function countSteps1(instructions: Instructions): number {
//     const { operations, map } = instructions;
//     const currentElements = [...map.keys()].filter((key) => key.match(/[A-Z]{2}A/)) as string[];
//     console.log(currentElements);
//     let pointer = 0;
//     // const targetElement = 'ZZZ';
//     const insturctionsLength: number = operations.length;
//     while (currentElements.find((element) => !element.match(/[A-Z]{2}Z/))) {
//         console.log(currentElements);
//         currentElements.filter((element) => !element.match(/[A-Z]{2}Z/)).forEach((element,i) => {
//             // if (/[A-Z]{2}Z/.test(element)) {
//             //     currentElements.splice(i, 1);
//             //     return;
//             // }
//             const [left, right] = map.get(element) as [string, string];
//             if (operations[pointer%insturctionsLength] === 'L') {
//                 currentElements[i] = left;
//             } else {
//                 currentElements[i] = right;
//             }    
//         });
//         pointer++;
//     }
//     return pointer;
// }

function parseInput(fileInput: string): Instructions {
    const map = new Map<string, [string, string]>();
    const [rawOps, rawMap] = fileInput.split('\n\n');
    const ops = rawOps.split('');

    rawMap.split('\n').forEach((line) => {
        const [key, value] = line.split(' = ');
        const [left, right] = value.match(/[A-Z0-9]+/g) as [string, string];
        map.set(key, [left, right]);
    });
    return { operations: ops, map };
}

function isPrime(number: number): boolean {
    if (number === 2) {
        return true;
    }
    if (number % 2 === 0) {
        return false;
    }
    for (let i = 3; i < Math.sqrt(number); i += 2) {
        if (number % i === 0) {
            return false;
        }
    }
    return true;
}