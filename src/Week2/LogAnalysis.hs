{-# OPTIONS_GHC -Wall #-}

module Week1.LogAnalysis where

import Week2.Log

{-
 Exercise 1 The first step is figuring out how to parse an individual message.
 Define a function
    parseMessage :: String -> LogMessage
 which parses an individual line from the log file.

 For example,
   parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-}
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : timestamp : info)         -> LogMessage Info (read timestamp) (unwords info)
  ("W" : timestamp : info)         -> LogMessage Warning (read timestamp) (unwords info)
  ("E" : level : timestamp : info) -> LogMessage (Error $ read level) (read timestamp) (unwords info)
  info -> Unknown (unwords info)


{-
 Exercise 2: Now that the above function can parse a single line. Next function should
 parse the whole Log file

 Define a function
   parse :: String -> [LogMessage]
 which parses an entire log file at once and returns its contents as a list of LogMessages
-}
parse :: String -> [LogMessage]
parse = map (parseMessage) . lines
