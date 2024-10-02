module Main where

data ItemType = Weapon | Armor | Ring deriving (Show)
data Item = Item { itemType :: ItemType, name :: String, damage :: Int, armor :: Int, cost :: Int } deriving (Show)
type Equipment = (Item, Item, Item, Item)
type Player = (Int, Int, Int, Int)

availableWeapons :: [Item]
availableWeapons = [
    Item Weapon "Dagger" 4 0 8, 
    Item Weapon "Shortsword" 5 0 10,
    Item Weapon "Warhammer" 6 0 25,
    Item Weapon "Longsword" 7 0 40,
    Item Weapon "Greataxe" 8 0 74]

availableArmors :: [Item]
availableArmors = [
    Item Armor "None" 0 0 0,
    Item Armor "Leather" 0 1 13,
    Item Armor "Chainmail" 0 2 31,
    Item Armor "Splintmail" 0 3 53,
    Item Armor "Bandedmail" 0 4 75,
    Item Armor "Platemail" 0 5 102]

availableRings :: [Item]
availableRings = [
    Item Ring "NoneLeft" 0 0 0,
    Item Ring "NoneRight" 0 0 0,
    Item Ring "Damage +1" 1 0 25,
    Item Ring "Damage +2" 2 0 50,
    Item Ring "Damage +3" 3 0 100,
    Item Ring "Defense +1" 0 1 20,
    Item Ring "Defense +2" 0 2 40,
    Item Ring "Defense +3" 0 3 80]

main :: IO ()
main = let  equipments = [ (w, a, r1, r2) | w <- availableWeapons, a <- availableArmors, r1 <- availableRings, r2 <- availableRings, name r1 /= name r2 ]
            getCost (_, _, _, c) = c
            players = map equipPlayer equipments
            winners = filter (willWin (109, 8, 2, 0)) players
            losers = filter (not . willWin (109, 8, 2, 0)) players in
    print (minimum $ map getCost winners, maximum $ map getCost losers)

equipPlayer :: Equipment -> Player
equipPlayer (Item Weapon _ damage _ cost, Item Armor _ _ armor cost', Item Ring _ dam' arm' cost'', Item Ring _ dam'' arm'' cost''') =
    (100, damage + dam' + dam'' , armor + arm' + arm'', cost + cost' + cost'' + cost''')

willWin :: Player -> Player -> Bool
willWin (health, damage, armor, _) (health', damage', armor', _) = 
    let damageDealtByBoss = max 1 (damage - armor')
        damageDealtByYou = max 1 (damage' - armor) in (health `div` damageDealtByYou) <= (health' `div` damageDealtByBoss)