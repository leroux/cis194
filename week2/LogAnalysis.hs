module LogAnalysis where

import Data.List hiding (insert)

import Log

-- Exercise 1
parseMessage ::  String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' m@(messageType:rest) = case messageType of
    "I" -> LogMessage Info timeStamp msg
    "W" -> LogMessage Warning timeStamp msg
    "E" -> LogMessage (Error errSeverity) errTimeStamp errMsg
    _ -> Unknown (unwords m)
  where timeStamp = read $ head rest :: Int
        msg = unwords $ tail rest
        errSeverity = read $ head rest :: Int
        errTimeStamp = read $ rest !! 1 :: Int
        errMsg = unwords $ drop 2 rest

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l Leaf = Node Leaf l Leaf
insert l (Node left x right)
  | l <= x = Node (insert l left) x right
  | otherwise = Node left x (insert l right)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr LogAnalysis.insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf x Leaf)  = [x]
inOrder (Node left x Leaf)  = inOrder left ++ [x]
inOrder (Node Leaf x right) = x : inOrder right
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMsg . sort . filter isSevereError
  where isSevereError (LogMessage (Error x) _ _) = x >= 50
        isSevereError _ = False
        extractMsg (LogMessage _ _ msg) = msg

-- Exercise 6
--  Mustardman or something? The guy must love mustard - he ate it all up.
