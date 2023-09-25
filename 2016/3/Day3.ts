const inputFile: string = Deno.args[0];
const input: string = await Deno.readTextFile(inputFile);
const lines: string[] = input.split("\r\n");

console.log(`Part 1: ${countValidTriangles(lines)}`);
console.log(`Part 2: ${countValidTriangles(lines, true)}`);

function parseLine(line: string): [number, number, number] {
  const [a, b, c] = line.split(" ").filter(elem => elem.length > 0).map((s) => Number(s));
  return [a, b, c];
}

function isTriangleValid(a: number, b: number, c: number): boolean {
  return a + b > c && a + c > b && b + c > a;
}

function countValidTriangles(lines: string[], vertical?: boolean): number {
    const formattedInput = formatInput(lines, vertical);
    let count = 0;
    while (formattedInput.length > 0) {
        const [a, b, c] = formattedInput.splice(0, 3);
        if (isTriangleValid(a, b, c)) count++;
    }
    return count;
}

function formatInput(lines: string[], vertical?: boolean): number[] {
    const numbers: number[] = [];
    if (vertical) {
        const inputLength = lines.length;
        for (let i = 0; i < inputLength; i++) {
            const [a1, b1, c1] = parseLine(lines[i]);
            numbers[i] = a1;
            numbers[i + inputLength] = b1;
            numbers[i + 2 * inputLength] = c1;
        }
        return numbers;
    }
    lines.forEach((line) => {
        const [a, b, c] = parseLine(line);
        numbers.push(a);
        numbers.push(b);
        numbers.push(c);
    });
    return numbers;
}