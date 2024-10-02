module Main where

maxMana = 1400

data BattleState = BattleState {
    playerHitPoints :: Int,
    playerMana :: Int,
    playerArmor :: Int,
    playerManaSpent :: Int,
    bossHitPoints :: Int,
    bossAttackDamage :: Int,
    shieldTurns :: Int,
    poisonTurns :: Int,
    rechargeTurns :: Int
} deriving (Show)

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show)

castSpell :: BattleState -> Spell -> BattleState
castSpell state MagicMissile = state { playerMana = playerMana state - 53, playerManaSpent = playerManaSpent state + 53, bossHitPoints = bossHitPoints state - 4 }
castSpell state Drain = state { playerMana = playerMana state - 73, playerManaSpent = playerManaSpent state + 73, bossHitPoints = bossHitPoints state - 2, playerHitPoints = playerHitPoints state + 2 }
castSpell state Shield = state { playerMana = playerMana state - 113, playerManaSpent = playerManaSpent state + 113, shieldTurns = 6 }
castSpell state Poison = state { playerMana = playerMana state - 173, playerManaSpent = playerManaSpent state + 173, poisonTurns = 6 }
castSpell state Recharge = state { playerMana = playerMana state - 229, playerManaSpent = playerManaSpent state + 229, rechargeTurns = 5 }

playTurn :: BattleState -> Bool -> Int
playTurn state playerTurn
    | bossHitPoints state <= 0 = playerManaSpent state
    | playerHitPoints state <= 0 = maxMana
    | playerMana state < 53 = maxMana
    | playerManaSpent state >= maxMana = maxMana
    | otherwise =
    let state' = applyEffects state in
        if playerTurn then minimum [ playTurn (castSpell state' spell) False | spell <- [MagicMissile, Drain, Shield, Poison, Recharge], spellCost spell <= playerMana state' ]
        else playTurn ((state' { playerHitPoints = playerHitPoints state' - max 1 (bossAttackDamage state' - playerArmor state') })) True
    where spellCost MagicMissile = 53
          spellCost Drain = 73
          spellCost Shield = 113
          spellCost Poison = 173
          spellCost Recharge = 229

applyEffects :: BattleState -> BattleState
applyEffects state = state {
    playerArmor = if shieldTurns state > 0 then 7 else 0,
    bossHitPoints = bossHitPoints state - if poisonTurns state > 0 then 3 else 0,
    playerMana = playerMana state + if rechargeTurns state > 0 then 101 else 0,
    shieldTurns = max 0 (shieldTurns state - 1),
    poisonTurns = max 0 (poisonTurns state - 1),
    rechargeTurns = max 0 (rechargeTurns state - 1) }

main :: IO ()
main = print (playTurn (BattleState 50 500 0 0 55 8 0 0 0) True,
    playTurn (BattleState 49 500 0 0 55 9 0 0 0) True )
