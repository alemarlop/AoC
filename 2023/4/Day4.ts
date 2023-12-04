interface Card {
    id: number;
    winningNumbers: Set<number>
    actualNumbers: Set<number>
}

const fileName: string = Deno.args[0];
const rawInput: string = await Deno.readTextFile(fileName);

const rawCards: string[] = rawInput.split('\n');

console.log('Part 1:', parseCards(rawCards).reduce((sum, curr) => sum + getCardValue(curr), 0));
console.log('Part 2:', countCards(parseCards(rawCards)));

function getCardValue(card: Card): number {
    return [...card.winningNumbers].reduce((total, current) => card.actualNumbers.has(current) ? Math.max(total*2, 1) : total, 0);
}

function countCards(cards: Card[]): number {
    const cardCount: Map<number, number> = new Map();
    cards.forEach(card => cardCount.set(card.id, 1));
    cards.forEach((card, index) => {
        const numCardsToAdd = [...card.winningNumbers].reduce((total, current) => card.actualNumbers.has(current) ? total + 1 : total, 0);
        const cardsToAdd = [...cards].slice(index+1, index + numCardsToAdd + 1);
        cardsToAdd.forEach(cardToAdd => {
            cardCount.set(cardToAdd.id, cardCount.get(cardToAdd.id) as number + (cardCount.get(card.id) as number))
        });
    });
    return [...cardCount.values()].reduce((sum, curr) => sum + curr, 0);
}

function parseCards(rawCards: string[]): Card[] {
    return rawCards.map(rawCard => {
        const [rawId, numbers] = rawCard.split(':');
        const [winningNumbers, actualNumbers] = numbers.split('|');
        return {
            id: Number((rawId.match(/\d+/) as RegExpMatchArray)[0]),
            winningNumbers: new Set((winningNumbers.match(/\d+/g) as RegExpMatchArray).map(num => Number(num))),
            actualNumbers: new Set((actualNumbers.match(/\d+/g) as RegExpMatchArray).map(num => Number(num)))
        }
    });
}