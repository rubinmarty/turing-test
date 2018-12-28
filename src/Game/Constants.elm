module Game.Constants exposing (..)


gameName : String
gameName =
    "TURING TEST"


helpMessage : List String
helpMessage =
    [ "Welcome to " ++ gameName ++ ", the build-your-own-Turing-machine game."
    , "Your goal is to design a turing machine capable of recognizing different types of words."
    , "Your machine has two pieces of memory: (1) its state, and (2) its tape."
    , "Based on the current state and the currently highlighted tape-cell, your machine will do three things:"
    , "    (a) Change what state it is in"
    , "    (b) Move right or left"
    , "    (c) Write a new value onto the tape-cell it just exited"
    , "With just these tools, your machine is capable of solving any problem that is solvable to an ordinary computer. Neat, huh?"
    , "Good luck!"
    ]
