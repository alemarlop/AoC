const fileName: string = Deno.args[0];
const input: string = await Deno.readTextFile(fileName);
const alphabet: string[] = 'abcdefghijklmnopqrstuvwxyz'.split('');

const rooms: string[] = input.split("\r\n");

console.log(`Part 1: ${sumValidOnes(rooms)}`)
console.log(`Part 2: ${decryptRooms(rooms)}`)

function sumValidOnes(rooms: string[]): number {
    return rooms.reduce((acc, curr) => acc+=isValid(curr), 0);
}

function decryptRooms(rooms: string[]): number {
    for (const room of rooms) {
        if (!isValid(room)) continue;
        const id = Number((/\d+/.exec(room) as string[])[0]);
        let body = room.split('-');
        body = body.slice(0, body.length - 1);
        const bodyAsString = body.join('-');
        const roomName = getRoomName(bodyAsString, id);
        if (roomName.includes('north')) return id;
    }
    return -1;
}

function getRoomName(room: string, id: number): string {
    const characters = room.split('');
    let res = '';
    for (const character of characters) {
        if (character === '-') res += ' ';
        else {
            const charCode = alphabet.indexOf(character);
            const newCharCode = (charCode + id) % alphabet.length;
            res += alphabet[newCharCode];
        }
    }
    return res;
}

function isValid(room: string): number {
    const id = Number((/\d+/.exec(room) as string[])[0]);
    const checksum = (/(?<=\[).+(?=\])/.exec(room) as string[])[0];
    let body = room.split('-');
    body = body.slice(0, body.length - 1);
    const parsedBody = body.join('').split('');
    const visited: Map<string, number> = new Map();
    parsedBody.forEach(character => {
        if (visited.has(character)) visited.set(character, (visited.get(character) as number) +1);
        else visited.set(character, 1);
    });
    const characters: string[] = Array.from(visited.keys());
    
    const orderedCharacters: string[] = characters.sort((a, b) => {
        const timesA: number = visited.get(a) as number;
        const timesB: number = visited.get(b) as number;
        if (timesA < timesB) return 2;
        if (timesA > timesB) return -2;
        return a < b ? -1 : 1;
    });

    if (orderedCharacters.slice(0,5).join('') === checksum) return id;
    return 0;
}
