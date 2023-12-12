interface Line {
    chain: string;
    built: string;
    values: number[];
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);

const lines: Line[] = fileContent.split('\n').map((line: string) => {
    const [rawchain, rawvalues] = line.split(' ');
    const values = rawvalues.split(',').map((value: string) => Number(value));
    return {
        chain: rawchain,
        built: '',
        values
    };
});

console.log('Part 1:', lines.reduce((a, b) => a + countPossibilities(b, 0), 0));
console.log('Part 2:', lines.map(line => expandLine(line, 5)).reduce((a, b) => a + countPossibilities(b, 0), 0));

function countPossibilities(line: Line, count: number, map: Map<string, number> = new Map<string, number>()): number {
    if (line.values.length === 0 && line.chain.length === 0) {
        return 1;
    } else if (line.values.length === 0) {
        return line.chain.includes('#') ? 0 : 1;
    }
    const currentTarget = line.values[0];

    if (currentTarget < count) {
        return 0
    };
    if (line.chain.length === 0) {
        return (line.values.length === 0 || (line.values.length === 1 && count === line.values[0])) ? 1 : 0;
    }
    const currentValue = line.chain.charAt(0);
    
    if (currentTarget === count && currentValue === '.') line = { ...line, values: line.values.slice(1) };
    switch (currentValue) {
        case '.': {
            if (count > 0 && count < currentTarget) return 0;
            return countPossibilities({ ...line, built: line.built += '.', chain: line.chain.substring(1)}, 0, map);
        }
        case '#': return countPossibilities({ ...line, built: line.built += '#', chain: line.chain.substring(1)}, count + 1, map);
        case '?': {
            const key = JSON.stringify(line.chain)+JSON.stringify(line.values);
            if (map.has(key)) {
                return map.get(key) as number;
            } else {
                const combined = countPossibilities({ ...line, chain: '#' + line.chain.substring(1)}, count, map) 
                    + countPossibilities({ ...line, chain: '.' + line.chain.substring(1)}, count, map);
                if (count === 0) {
                    map.set(key, combined);
                }
                return combined;
            }
        }        
    }
    return 1;
}

function expandLine(line: Line, times: number): Line {
    let values: number[] = [];
    const chains = [];
    for (let i = 0; i < times; i++) {
        values = values.concat(line.values);
        chains.push(line.chain);
    }
    return {
        chain: chains.join('?'),
        built: '',
        values,
    };
}