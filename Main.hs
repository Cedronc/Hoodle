import Control.Monad (guard, (>=>))
import Data.Maybe (fromJust)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Text.Read (readMaybe)
import Hoodle
import Pool
import Data.Time (LocalTime)
import LazyHoodle

-- IO code

promptInputString :: String -> IO String
promptInputString msg = putStrLn msg >> putStr ">>> " >> hFlush stdout >> getLine

promptInput :: (Read a) => String -> IO a
promptInput msg = loop
  where
    loop = do
      input <- promptInputString msg
      case readMaybe input of
        Nothing -> putStrLn "Invalid input; please try again." >> loop
        Just res -> return res

promptChoice :: String -> [(String, IO a)] -> IO a
promptChoice msg choices = loop
  where
    numberedChoices = zipWith (\i (choice, _) -> "[" ++ show i ++ "] " ++ choice) [1 ..] choices
    numberOfChoices = length numberedChoices
    loop = do
      input <- promptInputString $ init $ unlines (msg : numberedChoices)
      case readMaybe input of
        Just n
          | n >= 1 && n <= numberOfChoices ->
              snd $ choices !! (n - 1)
        _ -> putStrLn "Invalid choice" >> loop

promptChoice' :: [(String, IO a)] -> IO a
promptChoice' = promptChoice "Please make your selection:"

run :: (Pool p, Hoodle d, Ord t, Read t, Enum k, Ord k, Read k, Show k, Show (d t)) => p k (d t) -> IO ()
run db = putStrLn "Welcome to Hoodle!" >> mainMenu db
  where
    mainMenu db =
      promptChoice'
        [ ("View hoodles", viewHoodles db),
          ("Create a new hoodle", createHoodle db),
          ("Login as user", login db),
          ("Exit", return ())
        ]
    createHoodle db = do
      title <- promptInputString "Enter a title for your hoodle:"
      setupHoodle (initialize title) db
    viewHoodles db =
      promptChoice "Select the hoodle you want to view (or choose \"Cancel\" to go back)" $
        map
          ( \key ->
              let hoodle = fromJust (getFromPool key db)
               in (title hoodle ++ " (Hoodle ID: " ++ show key ++ ")", viewHoodle hoodle db)
          )
          (keys db)
          ++ [("Cancel", mainMenu db)]
    viewHoodle hoodle db = putStr (show hoodle) >> mainMenu db
    setupHoodle hoodle db = do
      putStr $ show hoodle
      promptChoice'
        [ ("Add a timeslot", addSlotToHoodle hoodle db),
          ("Remove a timeslot", removeSlotFromHoodle hoodle db),
          ("Save hoodle to pool", saveHoodle hoodle db),
          ("Cancel", mainMenu db)
        ]
    addSlotToHoodle hoodle db = do
      startTime <- promptInput "Enter start time for slot:"
      endTime <- promptInput "Enter end time for slot:"
      let slot = (startTime, endTime)
      hoodle' <- case add slot hoodle of
        Just d -> putStrLn "Slot added." >> return d
        Nothing -> putStrLn "Failed to add slot." >> return hoodle
      setupHoodle hoodle' db
    removeSlotFromHoodle hoodle db = do
      pos <- promptInput "Enter index of slot to remove"
      hoodle' <- case remove pos hoodle of
        Just d -> putStrLn "Slot removed." >> return d
        Nothing -> putStrLn "Invalid index." >> return hoodle
      setupHoodle hoodle' db
    saveHoodle hoodle db =
      let (k, db') = addToPool hoodle db
       in putStrLn ("Hoodle created! Hoodle ID: " ++ show k) >> mainMenu db'
    login db = do
      name <- promptInputString "Please enter your name:"
      userMenu name db
    userMenu name db = do
      putStrLn $ "Currently logged in as: " ++ name
      promptChoice'
        [ ("Register for a hoodle", registerUser name db),
          ("Unregister from a hoodle", unregisterUser name db),
          ("Vote for a hoodle slot", voteForUser name db),
          ("Go back", mainMenu db)
        ]
    modifyHoodle f db = do
      key <- promptInput "Enter the ID of the hoodle:"
      case getFromPool key db of
        Nothing -> putStrLn "Invalid ID." >> return db
        Just hoodle -> do
          result <- f hoodle
          case result of
            Nothing -> putStrLn "Failed." >> return db
            Just hoodle' -> putStrLn "Done." >> return (fromJust $ updatePool key hoodle' db)
    registerUser name = modifyHoodle (return . Just . register name) >=> userMenu name
    unregisterUser name = modifyHoodle (return . Just . unregister name) >=> userMenu name
    voteForUser name = modifyHoodle (\hoodle -> putStr (show hoodle) >> toggle name <$> promptInput "Enter index of the slot to toggle:" <*> pure hoodle) >=> userMenu name

-- main = run (emptyPool :: MyPool MyKey (MyDoodle MyTime))
main = run (emptyPool :: MapPool Int (LazyHoodle LocalTime))
