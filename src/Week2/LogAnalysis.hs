{-# OPTIONS_GHC -Wall #-}

module Week1.LogAnalysis where

import Week2.Log
import Text.Read
import Data.Maybe


{-
 Exercise 1 The first step is figuring out how to parse an individual
 message.

 Define a function
   parseMessage :: String -> LogMessage
 which parses an individual line from the log file.
 For example,
   parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
   parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
   parseMessage "This is not in the right format" == Unknown "This is not in the right format"

 Once we can parse one log message, we can parse a whole log file.
 Define a function
   parse :: String -> [LogMessage]
 which parses an entire log file at once and returns its contents as a
 list of LogMessages.

 To test your function, use the testParse function provided in the
 Log module, giving it as arguments your parse function, the number
 of lines to parse, and the log file to parse from (which should also be
 in the same folder as your assignment). For example, after loading
 your assignment into GHCi, type something like this at the prompt:
   testParse parse 10 "error.log"

 Don’t reinvent the wheel! (That’s so last week.) Use Prelude functions to make
 your solution as concise, high-level, and functional as
 possible. For example, to convert a String like "562" into an Int, you
 can use the read function. Other functions which may (or may not)
 be useful to you include lines, words, unwords, take, drop, and (.).
-}
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : timestamp : info) -> case (read timestamp :: Int) > 0 of
    True -> LogMessage Info (read timestamp) (unwords info)
    _ -> Unknown msg
  ("W" : timestamp : info) -> case (read timestamp :: Int) > 0 of
    True -> LogMessage Warning (read timestamp) (unwords info)
    _ -> Unknown msg
  ("E" : level : timestamp : info) -> case (read level :: Int) > 0
                                           && (read timestamp :: Int) < 101 of
    True -> LogMessage (Error $ read level) (read timestamp) (unwords info)
    _ -> Unknown msg
  info -> Unknown (unwords info)


-- Parse an entire log file at once and return its contents as a list of LogMessages
parse :: String -> [LogMessage]
parse = map (parseMessage) . lines


-------- Putting the logs in order ------------

{-
 Exercise 2: Define a function
    insert :: LogMessage -> MessageTree -> MessageTree

 which inserts a new LogMessage into an existing MessageTree, producing a new MessageTree.
 Insert may assume that it is given a sorted MessageTree, and must produce a new sorted
 MessageTree containing the new LogMessage in addition to the contents of the
 original MessageTree.

 However, note that if insert is given a LogMessage which is Unknown, it should
 return the MessageTree unchanged.
-}
insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert msg1@(LogMessage _ t1 _ ) tree@(Node left msg2@(LogMessage _ t2 _) right)
  | t2 > t1   = Node (insert msg1 left) msg2 right
  | t2 < t1   = Node left msg2 (insert msg1 right)
  | otherwise = tree


{-
 Exercise 3: Once we can insert a single LogMessage into a MessageTree,
 we can build a complete MessageTree from a list of messages. Specifically, define a function
    build :: [LogMessage] -> MessageTree

 which builds up a MessageTree containing the messages in the list,
 by successively
-}
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


{-
 Exercise 4: Finally, define the function
   inOrder :: MessageTree -> [LogMessage]

 which takes a sorted MessageTree and produces a list of all the
 LogMessages it contains, sorted by timestamp from smallest to biggest.
 (This is known as an in-order traversal of the MessageTree.)

 With these functions, we can now remove Unknown messages and
 sort the well-formed messages using an expression such as:
   inOrder (build tree)
-}
inOrder :: MessageTree -> [LogMessage]
-- base case
inOrder Leaf                  = []
-- Visit in the order: Left subtree -> root -> right subtree
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


{-
 Exercise 5: Now that we can sort the log messages, the only thing
 left to do is extract the relevant information. We have decided that
 “relevant” means “errors with a severity of at least 50”.

 Write a function
    whatWentWrong :: [LogMessage] -> [String]
 which takes an unsorted list of LogMessages, and returns a list of the
 messages corresponding to any errors with a severity of 50 or greater,
 sorted by timestamp. (Of course, you can use your functions from the
 previous exercises to do the sorting.)

 For example, suppose our log file looked like this:
  I 6 Completed armadillo processing
  I 1 Nothing to report
  E 99 10 Flange failed!
  I 4 Everything normal
  I 11 Initiating self-destruct sequence
  E 70 3 Way too many pickles
  E 65 8 Bad pickle-flange interaction detected
  W 5 Flange is due for a check-up
  I 7 Out for lunch, back in two time steps
  E 20 2 Too many pickles
  I 9 Back from lunch

 This file is provided as sample.log. There are four errors, three of
 which have a severity of greater than 50. The output of whatWentWrong
 on sample.log ought to be

  [ "Way too many pickles"
    , "Bad pickle-flange interaction detected"
    , "Flange failed!"
  ]

 You can test your whatWentWrong function with testWhatWentWrong,
 which is also provided by the Log module. You should provide
 testWhatWentWrong with your parse function, your whatWentWrong
 function, and the name of the log file to parse.
-}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = mapMaybe (readMaybe getMsg) $ filter important $ (inOrder . build) msgs
    where important (LogMessage (Error s) _ _) = s >= 50
          important _ = False
          getMsg (LogMessage _ _ msg) = msg
          getMsg _                    = ""
