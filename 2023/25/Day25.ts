const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);

function parseInput(input: string): Set<string> {
  const result = new Set<string>();
    input.split('\n').forEach(line => {
        const [from, to] = line.split(': ');
        to.split(' ').forEach(pssTo => { 
            if (result.has(`${pssTo}-${from}`)) return;
            result.add(`${from}-${pssTo}`);
        });
    });
    return result;
}

function getReachableComponents(start: string, components: Set<string>): string[] {
    const currentComponents = [...components.values()].filter(c => c.startsWith(start) || c.endsWith(start));
    const visitedComponents = new Set<string>();
    const result = new Set<string>();
    while (currentComponents.length > 0) {
        const currentComponent = currentComponents.shift() as string;
        if (visitedComponents.has(currentComponent)) continue;
        visitedComponents.add(currentComponent);
        const [from, to] = currentComponent.split('-');
        currentComponents.push(...[...components.values()].filter(c => c.startsWith(to) || c.endsWith(to) || c.startsWith(from) || c.endsWith(from)));
    }
    visitedComponents.forEach(c => {
        result.add(c.split('-')[0])
        result.add(c.split('-')[1])
    });
    return [...result.values()];
}

function testWires(components: Set<string>) {
    const allComponents = getReachableComponents([...components.values()][0].split('-')[0], components);
    const sortedComponents = [...components.values()].sort((a, b) => {
        const scoreA1 = [...components.values()].filter(val => val.startsWith(a.split('-')[0]) || val.endsWith(a.split('-')[0])).length;
        const scoreA2 = [...components.values()].filter(val => val.startsWith(a.split('-')[1]) || val.endsWith(a.split('-')[1])).length;
        const scoreA = scoreA1 + scoreA2;
        const scoreB1 = [...components.values()].filter(val => val.startsWith(b.split('-')[0]) || val.endsWith(b.split('-')[0])).length;
        const scoreB2 = [...components.values()].filter(val => val.startsWith(b.split('-')[1]) || val.endsWith(b.split('-')[1])).length;
        const scoreB = scoreB1 + scoreB2;
        return scoreA - scoreB;
    });
    console.log(sortedComponents);
    const baseLength = allComponents.length;
    const visitedComponents = new Set<string>();
    let result: number = 0;
    let count = 0;
    console.log(`Base length: ${baseLength}`);
    sortedComponents.forEach(c1 => {
        const possibleComponents = [...components.values()].filter(c => !c.startsWith(c1) && !c.endsWith(c1));
        possibleComponents.forEach(c2 => {
            if (c1 === c2) return;
            const possibleComponents2 = [...components.values()].filter(c => !c.startsWith(c2) && !c.endsWith(c2));
            possibleComponents2.forEach(c3 => {
                count++;
                // console.log(count);
                // if (count % 100000 === 0) console.log(count);
                if (c1 === c3 || c2 === c3) return;
                if (visitedComponents.has(`${c1}-${c2}-${c3}`) || 
                    visitedComponents.has(`${c1}-${c3}-${c2}`) ||
                    visitedComponents.has(`${c2}-${c3}-${c1}`) ||
                    visitedComponents.has(`${c2}-${c1}-${c3}`) ||
                    visitedComponents.has(`${c3}-${c2}-${c1}`) ||
                    visitedComponents.has(`${c3}-${c1}-${c2}`)) return;

                const newSet = new Set([...components.values()].filter(c => c !== c1 && c !== c2 && c !== c3));
                const newComponents = getReachableComponents([...newSet.values()][0].split('-')[0], newSet);
                if (newComponents.length < baseLength) {
                    console.log(`New length: ${newComponents.length}`);
                    console.log(`Removed: ${c1}, ${c2}, ${c3}`);
                    const otherComponent = allComponents.find(c => newComponents.indexOf(c) === -1) as string;
                    console.log(`Other component: ${otherComponent}`);
                    const otherComponents = getReachableComponents(otherComponent, newSet);
                    if (newComponents.length + otherComponents.length === baseLength) {
                        console.log(`Solution found: ${otherComponents.length, newComponents.length}`);
                        result = otherComponents.length * newComponents.length;
                        console.log(`Result: ${result}`)
                        return;
                    }
                }
                visitedComponents.add(`${c1}-${c2}-${c3}`);
            });
        });
    });
}

// function reverseComponent(component: string): string {
//     const [from, to] = component.split('-');
//     return `${to}-${from}`;
// }

console.log(testWires(parseInput(fileContent)));
// console.log(parseInput(fileContent));