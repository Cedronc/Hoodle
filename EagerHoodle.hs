module EagerHoodle where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq, deleteAt, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set, delete, insert, member)
import qualified Data.Set as Set
import Data.Time.LocalTime (LocalTime)
import Hoodle
import LazyHoodle (toTuple)
import Control.Concurrent (threadDelay)
slowFunc' = threadDelay 2000000 >> return 10

-- TODO: Create a function for forcing evaluation of parameters, so its fully unthunkefied.

data Timeslot t = Timeslot {start :: t, end :: t, attendees :: Set String} deriving (Show)

data EagerHoodle t = EagerHoodle
  { name :: String,
    --    time, time, attendees
    ts :: Seq (Timeslot t),
    ps :: Set String
  } deriving (Show)

instance Hoodle EagerHoodle where
  -- ts, ps are already forced during evaluating of `|>` operations (and others)
  add (!tStart, !tEnd) (EagerHoodle !title ts ps)
    | tStart == tEnd = Nothing -- TODO: Double check if this is even necessary
    | any (overlaps (tStart, tEnd)) ts = Nothing
    | otherwise = Just (EagerHoodle title (a ts tStart tEnd) ps)
    where
      overlaps (tStart, tEnd) (Timeslot slotStart slotEnd _) = tStart < slotEnd && tEnd > slotStart
      a ts tStart tEnd = ts |> Timeslot tStart tEnd Set.empty

  remove !slotpos (EagerHoodle !title !ts !ps) = case Seq.lookup slotpos ts of
    Just slot -> Just (EagerHoodle title (deleteAt slotpos ts) ps)
    Nothing -> Nothing


  register !usr (EagerHoodle !a !b ps) = EagerHoodle a b (insert usr ps)
  unregister !usr (EagerHoodle !a !b ps) = EagerHoodle a b (delete usr ps)
  toggle !usr !slotpos (EagerHoodle !a ts ps)
    | Seq.length ts <= slotpos = Nothing
    -- Remove flow
    | member usr $ attendees slot =
        let updatedAttendees = (delete usr $ attendees slot)
         in Just (EagerHoodle a (f updatedAttendees) ps)
    -- Add flow
    | otherwise =
        let updatedAttendees = insert usr $ attendees slot
         in if member usr ps then Just (EagerHoodle a (f updatedAttendees) ps) else Nothing
    where
      slot = Seq.index ts slotpos
      f att = Seq.update slotpos (Timeslot (start slot) (end slot) att) ts
  title = name
  -- wtf is this withIndex
  -- slots = toList . Seq.mapWithIndex (\_ el -> toTuple el) . ts
  participants = Set.toList . ps
  -- Here we do force all fields of the Hoodle because there are no operations
  -- on the set or sequence
  busiestSlot h@(EagerHoodle !_ !ts !ps )
    | null sl = Nothing
    | all (\(_,_,a) -> null a) sl = Nothing
    | otherwise = Just (maximumBy (\(_,_,x) (_,_,y) -> compare (length x) (length y)) sl)
    where sl = slots h
