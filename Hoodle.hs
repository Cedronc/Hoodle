module Hoodle(Hoodle(..)) where 

class Hoodle h where
  initialize :: String -> h t
  add :: (Ord t) => (t, t) -> h t -> Maybe (h t)
  remove :: Int -> h t -> Maybe (h t)
  register :: String -> h t -> h t
  unregister :: String -> h t -> h t
  toggle :: String -> Int -> h t -> Maybe (h t)
  title :: h t -> String
  slots :: h t -> [(t, t, [String])]
  participants :: h t -> [String]
  busiestSlot :: h t -> Maybe (t,t, [String])
