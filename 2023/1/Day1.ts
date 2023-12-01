const fileName = Deno.args[0]
const input = await Deno.readTextFile(fileName);

const lines: string[] = input.split('\n');

console.log('Part 1:', decodeLines(lines, ['0','1','2','3','4','5','6','7','8','9']));
console.log('Part 2:', decodeLines(lines, 
    ['zero','one','two','three','four','five','six','seven','eight','nine','0','1','2','3','4','5','6','7','8','9']));

function decodeLines(lines: string[], writtenNumbers: string[]): number {
    return lines.reduce((previous: number, current: string) => {
        const presentElements = writtenNumbers.filter(wn => current.includes(wn))
        const [fst, _fstIdx, lst, _lstIdx] = presentElements.reduce((fstAndlst, currentPresentElement) => {
            if (current.indexOf(currentPresentElement) < (fstAndlst[1] as number)) {
                fstAndlst[1] = current.indexOf(currentPresentElement);
                fstAndlst[0] = currentPresentElement;
            }
            if (current.lastIndexOf(currentPresentElement) > (fstAndlst[3] as number)) {
                fstAndlst[3] = current.lastIndexOf(currentPresentElement);
                fstAndlst[2] = currentPresentElement;
            }
            return fstAndlst;
        }, ["", current.length, "", -1]);
        return previous + Number(`${writtenNumbers.indexOf(fst)%10}${writtenNumbers.indexOf(lst)%10}`);
    }, 0)
}
