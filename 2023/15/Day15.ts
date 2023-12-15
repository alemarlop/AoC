interface Element {
    label: string;
    focalLength: number;
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);
const steps: string[] = fileContent.split(",");
const boxes: Element[][] = [];

console.log("Part 1: " + steps.reduce((a, b) => a + hashStep(b, 0), 0));
console.log("Part 2: " + sumFocusingPower(populateBoxes(steps, boxes)));


function hashStep(step: string, acc: number): number {
    if (step.length === 0) {
        return acc;
    }
    const firstCode: number = step.charCodeAt(0);
    acc += firstCode;
    acc *= 17;
    acc %= 256;
    return hashStep(step.substring(1), acc);
}

function populateBoxes(input: string[], boxes: Element[][]): Element[][] {
    input.forEach((element: string) => {
        const label = (element.match(/([a-z]+)/) as RegExpMatchArray)[0];
        const target = hashStep(label, 0);
        if (boxes[target] === undefined) {
            boxes[target] = [];
        }
        if (!element.includes("=")) {
            boxes[target] = [...boxes[target]].filter((e: Element) => e.label !== label);
            return;
        }
        const focalLength = Number((element.match(/([0-9]+)/) as RegExpMatchArray)[0]);
        if (boxes[target].find((e: Element) => e.label === label)) {
            boxes[target] = [...boxes[target]].map((e: Element) => {
                return e.label !== label ? e : { label, focalLength };
            });
         } else {
            boxes[target].push({ label, focalLength });
         }
    });
    return boxes;
}

function sumFocusingPower(boxes: Element[][]): number {
    let acc = 0;
    for (let i = 0; i < boxes.length; i++) {
        if (boxes[i] === undefined) {
            continue;
        }
        boxes[i].forEach((box: Element, index: number) => {
            acc += box.focalLength * (i+1) * (index+1);
        });
    }
    return acc;
}