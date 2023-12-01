import { Ingredient } from "./Ingredient.ts";

const input: string = Deno.readTextFileSync('./test.txt');

const allIngredients: Ingredient[] = populateIngredients(input);
const allergens: string[] = [];

const possibleAllergensMap: Map<string, Ingredient[]> = new Map();


input.split('\n').forEach((element: string) => {
    /(?<=contains\s).+(?=\))/g.exec(element)?.[0].split(', ').forEach((allergen: string) => {
        
        allergens.push(allergen);
    });
});

console.log(allergens);

function populateIngredients(input: string): Ingredient[] {
    const allIngredients: Ingredient[] = [];
    input.split('\n').forEach((line: string) => {
        const ingredients: string[] = line.split(' (')[0].split(' ');
        ingredients.forEach((ingredient: string) => {
            const newIngredient: Ingredient = new Ingredient(ingredient);
            allIngredients.push(newIngredient);
        });
    });
    return allIngredients;
}