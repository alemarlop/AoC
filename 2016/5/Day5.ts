import { Md5 } from "https://deno.land/std@0.119.0/hash/md5.ts";

const roomId: string = Deno.args[0];

console.log(`Part 1: ${getRoomPassword(roomId)}`);
console.log(`Part 2: ${getRoomSecurePassword(roomId)}`);

function getRoomPassword(roomId: string): string {
    let password = "";
    let index = 0;
    while (password.length < 8) {
        const hash: string = new Md5().update(`${roomId}${index}`).toString("hex");
        if (hash.startsWith("00000")) password += hash[5];
        index++;
    }
    return password;
}

function getRoomSecurePassword(roomId: string): string {
    const password: string[] = [];
    let discoveredCharactersCount = 0;
    let index = 0;
    while (discoveredCharactersCount < 8) {
        const hash: string = new Md5().update(`${roomId}${index}`).toString("hex");
        if (hash.startsWith("00000")) {
            if (!isNaN(Number(hash[5])) && !password[Number(hash[5])] && 0 <= Number(hash[5]) && Number(hash[5]) <= 7) {
                discoveredCharactersCount++;
                password[Number(hash[5])] = hash[6];
            }
        }
        index++;
    }
    return password.slice(0,8).join('');
}
