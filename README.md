# Tower Defense Game

## Contributors

- Stepan Peredereev
- Igor Baranov
- Nikita Zelenov

---

## How to install and play

- clone this repository
- then run in the directory that contains project:
```
cabal update
cabal run
```

---

## Game Rules
- Firstly, you need to choose difficulty (Easy, Hard, Normal). It is recommended to choose Easy map for the first playing
- Then you have a time to place towers that will defend a path
- Enemies will appear as waves (from small to big),
- The last wave is a boss that have a lot of HP

---

## Towers
Towers should be placed in buildable (green) tiles.
Each type of tower costs a particular price and has its own characteristics:

- Cannon - normal damage, one target, large damage area, no additional effects for enemy, price: 50
- Slow - small damage, one target, small damage area, freeze enemy (enemy walks slower), price: 75
- Splash - high damage, multiple targets, normal damage area, no additional effects for enemy, price: 100

## Gates
- Gates - has HP, can block (damage) enemies while HP is greater than zero , price: 300
  Gates can be placed on the road tiles
---

## Enemies
Enemies follow the path (road (brown) tiles). 
Each type of enemy arrives at particular waves and has its own characteristics

- Basic enemy - small HP, normal speed, first appearance: 1 wave
- Strong enemy - normal HP, small speed, first appearance: 2 wave 
- Boss - a lot of HP, very small speed, first appearance: 4 wave

---

## Assets
images for enemies are taken from the open source, its are famous smiles from the Geometry Dash game
images for tiles are taken from Minecraft game
---