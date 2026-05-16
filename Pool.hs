module Pool where

import Data.Foldable
import Data.Sequence (Seq, deleteAt, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set, delete, insert, member)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hoodle (Hoodle (slots, remove, add, busiestSlot))
import LazyHoodle (LazyHoodle(..), Timeslot (..))
import Data.Maybe (fromJust)


findOptimalSchedule :: (Pool p, Show (t), Show i, Ord i, Ord t) => p i (LazyHoodle t) -> [LazyHoodle t]
findOptimalSchedule p = fmap (\h -> withSingleSlot h (busiestSlot h)) $ hoodles p
  -- where f p = concat $ slots <$> hoodles p

withSingleSlot :: (Ord t) => LazyHoodle t -> Maybe (t,t,[String]) -> LazyHoodle t
withSingleSlot h Nothing = h { ts = Seq.empty }
withSingleSlot h (Just (s,e,a)) = h { ts = Seq.singleton (Timeslot s e (Set.fromList a)) }
  where f n = remove 0 $ fromJust $ f (n - 1)


class Pool p where
  emptyPool :: p k a
  addToPool :: (Enum k, Ord k) => a -> p k a -> (k, p k a)
  updatePool :: (Ord k) => k -> a -> p k a -> Maybe (p k a)
  getFromPool :: (Ord k) => k -> p k a -> Maybe a
  keys :: p k a -> [k]
  hoodles :: p k a -> [a]

newtype MapPool k a = MapPool { pool :: Map k a } deriving (Show)

instance Pool MapPool where
  emptyPool = MapPool Map.empty
  addToPool a (MapPool pool) =
    let k = if Map.null pool
            then toEnum 0 -- wraps 0 in enum a type
            else succ $ maximum $ Map.keys pool
     in (k, MapPool (Map.insert k a pool))

  updatePool k a d = Just (MapPool (Map.alter (\_ -> Just a) k $ pool d))
  getFromPool = (. pool) . Map.lookup
  keys = Map.keys . pool
  hoodles (MapPool p) = snd <$> Map.toList p


myPool :: MapPool Int (LazyHoodle Int)
myPool = MapPool (Map.fromList [(0, tmpHoodle1), (1, tmpHoodle2), (2, tmpHoodle3), (3, tmpHoodle4), (4, hoodle0), (5, tmpHoodle5), (6, tmpHoodle6), (7, tmpHoodle7), (8, tmpHoodle8)])

-- fuck = findOptimalSchedule myPool

tmpHoodle1 =
  LazyHoodle
    "Team Standup"
    ( Seq.fromList
        [ Timeslot 9 10 (Set.fromList ["Alice", "Bob", "Charlie"]),
          Timeslot 11 12 (Set.fromList ["Alice", "Charlie"]),
          Timeslot 14 15 (Set.fromList ["Bob"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave"])

-- A lunch hoodle where not everyone is available
tmpHoodle2 =
  LazyHoodle
    "Team Lunch"
    ( Seq.fromList
        [ Timeslot 12 13 (Set.fromList ["Alice", "Dave"]),
          Timeslot 13 14 (Set.fromList ["Bob", "Charlie", "Dave"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave"])

-- An empty hoodle with participants but no slots yet
tmpHoodle3 =
  LazyHoodle
    "Kickoff Meeting"
    Seq.empty
    (Set.fromList ["Alice", "Bob"])

-- A hoodle with slots but no participants signed up yet
tmpHoodle4 =
  LazyHoodle
    "Workshop"
    ( Seq.fromList
        [ Timeslot 9 11 Set.empty,
          Timeslot 13 17 Set.empty
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie"])

hoodle0 = LazyHoodle "Empty" Seq.empty Set.empty

tmpHoodle5 =
  LazyHoodle
    "Sprint Planning"
    ( Seq.fromList
        [ Timeslot 10 12 (Set.fromList ["Alice", "Bob", "Charlie", "Dave"]),
          Timeslot 14 15 (Set.fromList ["Alice", "Bob"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave", "Eve"])

tmpHoodle6 =
  LazyHoodle
    "Code Review"
    ( Seq.fromList
        [ Timeslot 15 16 (Set.fromList ["Charlie", "Dave"]),
          Timeslot 16 17 (Set.fromList ["Alice", "Bob", "Eve"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave", "Eve"])

tmpHoodle7 =
  LazyHoodle
    "Design Review"
    ( Seq.fromList
        [ Timeslot 10 11 (Set.fromList ["Alice", "Bob", "Charlie"]),
          Timeslot 13 14 (Set.fromList ["Alice", "Bob"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave"])

-- Hoodle with timeslot 10-12 (3 attendees) overlapping with tmpHoodle7's 10-11
tmpHoodle8 =
  LazyHoodle
    "Architecture Meeting"
    ( Seq.fromList
        [ Timeslot 10 12 (Set.fromList ["Bob", "Charlie", "Dave"]),
          Timeslot 15 16 (Set.fromList ["Dave", "Eve"])
        ]
    )
    (Set.fromList ["Alice", "Bob", "Charlie", "Dave", "Eve"])
