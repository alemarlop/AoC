enum HandType {
    FIVE_OF_A_KIND,
    FOUR_OF_A_KIND,
    FULL_HOUSE,
    THREE_OF_A_KIND,
    TWO_PAIR,
    ONE_PAIR,
    HIGH_CARD
}

interface Hand {
    cards: string;
    bid: number;
    type: HandType;
}

const cardValues: string[] = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J' ,'Q', 'K', 'A'];
const cardValuesJoker: string[] = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T' ,'Q', 'K', 'A'];

const consequenceMap = {
    1: {
        [HandType.FOUR_OF_A_KIND]: HandType.FIVE_OF_A_KIND,
        [HandType.FULL_HOUSE]: HandType.FOUR_OF_A_KIND,
        [HandType.THREE_OF_A_KIND]: HandType.FOUR_OF_A_KIND,
        [HandType.TWO_PAIR]: HandType.FULL_HOUSE,
        [HandType.ONE_PAIR]: HandType.THREE_OF_A_KIND,
        [HandType.HIGH_CARD]: HandType.ONE_PAIR
    },
    2: {
        [HandType.FOUR_OF_A_KIND]: HandType.FIVE_OF_A_KIND,
        [HandType.FULL_HOUSE]: HandType.FIVE_OF_A_KIND,
        [HandType.THREE_OF_A_KIND]: HandType.FOUR_OF_A_KIND,
        [HandType.TWO_PAIR]: HandType.FOUR_OF_A_KIND,
        [HandType.ONE_PAIR]: HandType.THREE_OF_A_KIND,
        [HandType.HIGH_CARD]: HandType.THREE_OF_A_KIND
    },
    3: {
        [HandType.FOUR_OF_A_KIND]: HandType.FIVE_OF_A_KIND,
        [HandType.FULL_HOUSE]: HandType.FIVE_OF_A_KIND,
        [HandType.THREE_OF_A_KIND]: HandType.FOUR_OF_A_KIND,
        [HandType.TWO_PAIR]: HandType.FOUR_OF_A_KIND,
        [HandType.ONE_PAIR]: HandType.FULL_HOUSE,
        [HandType.HIGH_CARD]: HandType.THREE_OF_A_KIND
    }

};

const fileName: string = Deno.args[0];
const rawHands: string[] = (await Deno.readTextFile(fileName)).split('\n');

console.log('Part 1:', totalWinnings(rawHands, false));
console.log('Part 2:', totalWinnings(rawHands, true));

function totalWinnings(rawHands: string[], jokers: boolean): number {
    let hands = parseHands(rawHands);
    if (jokers) hands = hands.map(hand => manageJokers(hand));
    const orderedHands: Hand[] = orderHands(hands, jokers);
    return orderedHands.reduce((total, hand, index) => {
        return total + (hand.bid * (index + 1));
    }, 0);
}

function parseHands(rawHands: string[]): Hand[] {
    return rawHands.map(rawHand => {
        return getHand(rawHand);
    });
}

function getHand(rawHand: string): Hand {
    const [cards, bid]: string[] = rawHand.split(' ');
    const occurences = getOccurences(cards);

    if (occurences.size === 1) {
        return { cards, bid: Number(bid), type: HandType.FIVE_OF_A_KIND };
    }
    if (occurences.size === 2) {
        const [card1, card2]: string[] = [...occurences.keys()];
        const occurence1: number = occurences.get(card1) || 0;
        const occurence2: number = occurences.get(card2) || 0;

        if (occurence1 === 4 || occurence2 === 4) {
            return { cards, bid: Number(bid), type: HandType.FOUR_OF_A_KIND };
        }
        return { cards, bid: Number(bid), type: HandType.FULL_HOUSE };
    }

    if (occurences.size === 3) {
        const [card1, card2, card3]: string[] = [...occurences.keys()];
        const occurence1: number = occurences.get(card1) || 0;
        const occurence2: number = occurences.get(card2) || 0;
        const occurence3: number = occurences.get(card3) || 0;

        if (occurence1 === 3 || occurence2 === 3 || occurence3 === 3) {
            return { cards, bid: Number(bid), type: HandType.THREE_OF_A_KIND };
        }
        return { cards, bid: Number(bid), type: HandType.TWO_PAIR };
    }

    if (occurences.size === 4) {
        return { cards, bid: Number(bid), type: HandType.ONE_PAIR };
    }

    return { cards, bid: Number(bid), type: HandType.HIGH_CARD };
}

function manageJokers(hand: Hand): Hand {
    if (!hand.cards.includes('J') || hand.type === HandType.FIVE_OF_A_KIND) return hand;
    if (hand.type === HandType.FOUR_OF_A_KIND) return {...hand, type: HandType.FIVE_OF_A_KIND};
    const occurences = getOccurences(hand.cards);
    return {...hand, type: (consequenceMap as any)[occurences.get('J') as number][hand.type]};

}

function orderHands(hands: Hand[], jokers: boolean): Hand[] {
    let cardValuesToUse = cardValues;
    if (jokers) cardValuesToUse = cardValuesJoker;
    return hands.sort((h1, h2) => {
        if (h1.type !== h2.type) return h2.type - h1.type;
        for (let i = 0; i < h1.cards.length; i++) {
            const value = cardValuesToUse.indexOf(h1.cards[i]) - cardValuesToUse.indexOf(h2.cards[i]);
            if (value !== 0) return value;
        }
        return 0;
    });

}

function getOccurences(cards: string): Map<string, number> {
    const occurences: Map<string, number> = new Map();
    cards.split('').forEach(card => {
        const occurence: number = occurences.get(card) || 0;
        occurences.set(card, occurence + 1);
    });
    return occurences;
}