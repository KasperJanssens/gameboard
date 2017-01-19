#!/usr/bin/env stack
--stack runghc --resolver lts-7.14 --install-ghc --package bimap
import qualified Data.Map as M
import Data.Bimap

data Gameboard space player item = Gameboard (M.Map space [player]) (M.Map space [item]) (M.Map player space)

init:: Ord space => [space] -> Gameboard space player item
init spaces = 
  let spacePlayersMap = M.fromList $ zip spaces $ repeat [] in
  let spaceItemsMap = M.fromList $ zip spaces $ repeat [] in
  let playerSpaceMap = M.empty in
  Gameboard spacePlayersMap spaceItemsMap playerSpaceMap

fetchPlayers:: Ord space => space -> Gameboard space player item -> Maybe [player]
fetchPlayers space (Gameboard spacePlayerMap _ _ ) =
  M.lookup space spacePlayerMap

placePlayer:: (Ord space, Ord player) => space -> player -> Gameboard space player item -> Maybe (Gameboard space player item)
placePlayer space player gameboard@(Gameboard spacePlayerMap spaceItemMap playerSpaceMap) = do
  players <- fetchPlayers space gameboard
  let newPlayers = player:players
      newSpacePlayerMap =  M.insert space newPlayers spacePlayerMap
      newPlayerSpaceMap = M.insert player space playerSpaceMap
  return $ Gameboard newSpacePlayerMap spaceItemMap newPlayerSpaceMap

--movePlayerFromTo::(Ord space, Ord player) => space -> space -> player -> Gameboard space player item -> Maybe (Gameboard space player item)
--movePlayerFromTo from to player gameboard =
  

main = print "koekoek"
