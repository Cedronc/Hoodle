module LazyHoodle where

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

data Timeslot t = Timeslot {start :: t, end :: t, attendees :: Set String} deriving (Show)

-- Only used to comply with the typeclass slots function
toTuple :: Timeslot t -> (t, t, [String])
toTuple (Timeslot s e a) = (s, e, Set.toList a)
-- fromTuple :: (t, t, [String]) -> Timeslot t
-- fromTuple (s, e, a) = Timeslot s e (Set.fromList a)

-- TODO: Implement the strict Hoodle
data LazyHoodle t = LazyHoodle
  { name :: String,
    --    time, time, attendees
    ts :: Seq (Timeslot t),
    ps :: Set String
  }

instance (Show t) => Show (LazyHoodle t) where
  show (LazyHoodle name ts ps) =
    let lineWidth = case Seq.lookup 0 ts of
          Nothing -> 45
          Just (Timeslot s e ps) -> 2 * length (show s) + persWidth + 10
        -- TODO: Decide which version of longest to keep, I think longestSl and remove the persWidth 2026-04-22 15:50
        -- TODO: Hardcoded 2 character index for timeslots
        persWidth = length <$> maximum $ (\(Timeslot s e ps) -> unwords $ toList ps) <$> ts
        longestSl = maximumBy (\(Timeslot _ _ ps) (Timeslot _ _ ps') -> compare (length $ unwords $ toList ps') (length $ unwords $ toList ps)) (toList ts)
        seperator = case Seq.lookup 0 ts of
          Nothing -> "+-" ++ replicate lineWidth '-' ++ "+\n"
          Just (Timeslot s _ ps) -> "+---" ++ timeLen s ++ "+" ++ timeLen s ++ "+" ++ attnLen ps ++ "+\n"
            where
              timeLen s = replicate ((+2) $ length $ show s) '-'
              attnLen ps = replicate ((+2) persWidth) '-'

        nameLine = "| " ++ name ++ replicate (lineWidth - length name) ' ' ++ "|\n"
        membLine =
          let names = unwords $ toList ps
           in "| (Participants: " ++ names ++ ")" ++ replicate (lineWidth - length names - 18) ' ' ++ "  |\n"
        timeLines = let lines = concat $ Seq.mapWithIndex (\i (Timeslot s e ps) -> "| " ++ show i ++ ": " ++ show s ++ " | " ++ show e ++ " | " ++ makeUserStr ps ++ " |\n") ts
                    in if lines == "" then lines else lines ++ seperator
                    where makeUserStr ps = unwords (toList ps) ++ replicate (persWidth - length (unwords $ toList ps)) ' '
     in   "+" ++ replicate (1+lineWidth) '-' ++ "+\n"
          ++ nameLine
          ++ membLine
          ++ seperator
          ++ timeLines

instance Hoodle LazyHoodle where
  initialize str = LazyHoodle str Seq.empty Set.empty

  -- TODO: Clean up this add function
  add (tStart, tEnd) (LazyHoodle title ts ps)
    | tStart == tEnd = Nothing -- TODO: Double check if this is even necessary
    | any (overlaps (tStart, tEnd)) ts = Nothing
    | otherwise = Just (LazyHoodle title (a ts tStart tEnd) ps)
    where
      overlaps (tStart, tEnd) (Timeslot slotStart slotEnd _) = tStart < slotEnd && tEnd > slotStart
      a ts tStart tEnd = ts |> Timeslot tStart tEnd Set.empty

  -- TODO: Change to fmap from Data.Functor look at wpo8 (recheck if this can be done elsewhere)
  remove slotpos (LazyHoodle title ts ps) = case Seq.lookup slotpos ts of
    Just slot -> Just (LazyHoodle title (deleteAt slotpos ts) ps)
    Nothing -> Nothing

  register usr (LazyHoodle a b ps) = LazyHoodle a b (insert usr ps)
  unregister usr (LazyHoodle a b ps) = LazyHoodle a b (delete usr ps)
  toggle usr slotpos (LazyHoodle a ts ps)
    | Seq.length ts <= slotpos = Nothing
    -- Remove flow
    | member usr $ attendees slot =
        let updatedAttendees = (delete usr $ attendees slot)
         in Just (LazyHoodle a (f updatedAttendees) ps)
    -- Add flow
    | otherwise =
        let updatedAttendees = insert usr $ attendees slot
         in if member usr ps then Just (LazyHoodle a (f updatedAttendees) ps) else Nothing
    where
      slot = Seq.index ts slotpos
      f att = Seq.update slotpos (Timeslot (start slot) (end slot) att) ts
  title = name
  -- wtf is this withIndex
  slots = toList . Seq.mapWithIndex (\_ el -> toTuple el) . ts
  participants = Set.toList . ps
  busiestSlot h
    | null (slots h) = Nothing
    | all (\(_,_,a) -> null a) (slots h) = Nothing
    | otherwise = Just (maximumBy (\(_,_,x) (_,_,y) -> compare (length x) (length y)) $ slots h)

-- A team meeting hoodle with some participants signed up to slots
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

t = add (2, 3) $ fromJust $ add (1, 2) hoodle0
