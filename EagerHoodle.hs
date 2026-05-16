module EagerHoodle where

import Hoodle
import Data.Sequence (Seq, deleteAt, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set, delete, insert, member)
import qualified Data.Set as Set

-- findOptimalSchedule :: (Foldable t, Hoodle t, Show ls) => t ls -> String
-- findOptimalSchedule hs = (\ v -> show v) <$> hs

data Timeslot t = Timeslot {start :: t, end :: t, attendees :: Set String} deriving (Show)

data EagerHoodle t = EagerHoodle
  { name :: String,
    --    time, time, attendees
    ts :: Seq (Timeslot t),
    ps :: Set String
  } deriving (Show)
 
instance Hoodle EagerHoodle where
  add (tStart, tEnd) (EagerHoodle title ts ps)
    | tStart == tEnd = Nothing -- TODO: Double check if this is even necessary
    | any (overlaps (tStart, tEnd)) ts = Nothing
    | otherwise = Just (EagerHoodle title (a ts tStart tEnd) ps)
    where
      overlaps (tStart, tEnd) (Timeslot slotStart slotEnd _) = tStart < slotEnd && tEnd > slotStart
      a ts tStart tEnd = ts |> Timeslot tStart tEnd Set.empty

  remove slotpos (EagerHoodle title ts ps) = case Seq.lookup slotpos ts of
    Just slot -> Just (EagerHoodle title (deleteAt slotpos ts) ps)
    Nothing -> Nothing
