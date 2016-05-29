import Data.Char
import WordSet
import AATree
import Sentence

wordsets :: FilePath
wordsets = "wordsets.txt"

conversations :: FilePath
conversations = "conversations.txt"

prog :: IO() 
prog = do 
    putStr "Loading."
    wordsets <- readFile wordsets
    putStr "."
    conversations <- readFile conversations
    putStrLn "."
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    chat (read wordsets) (read conversations)
    
    {-
chat :: AATree (WordSet String) -> AATree (WordSet String) -> IO ()
chat tree1 tree2 = do
    putStrLn $show $size tree1
    let answer = (constructNaive (Just (rootVal tree1)) tree1)
    if tree1 /= emptyTree then putStrLn answer
    else putStrLn "Created a file."
    putStr "Please rate this answer from 1 to 5:> "
    score <- getLine
    
    putStr ":> "
    userInp <- getLine
    putStrLn $show $bestSentence userInp $map word $inorder tree2
    if userInp == "quit" then quit tree1 tree2
    else chat (addSent userInp tree1) tree2
    -}
    
chat :: AATree (WordSet String) -> AATree (WordSet String) -> IO()
chat sets sents = do
        putStrLn ""
        putStr ":> "
        userInp <- getLine
        if userInp == "quit" then quit sets sents else do
         let closestInp = bestSentence userInp $map word $inorder sents
         let oldAnswer = successor $fromJustWordSet (getWord' (fst closestInp) sents)
         let newAnswer = (constructNaive (Just (rootVal sets)) sets)
         putStr "Lillebil says: "
         if snd closestInp > 0.5 then putStrLn oldAnswer else putStrLn newAnswer
         putStrLn ""
         putStr "Match with score "
         putStr $show $snd closestInp
         putStr " - "
         putStrLn $fst closestInp
         putStr "Please rate this answer from 0 to 5 :> "
         score <- getLine
         putStrLn "Thank you!"
         if snd closestInp > 0.5 
          then chat (addSent userInp sets) (addAnswer userInp oldAnswer (read score) sents)
          else chat (addSent userInp sets) (addAnswer userInp newAnswer (read score) sents)

    
quit :: AATree (WordSet String) -> AATree (WordSet String) -> IO ()
quit tree1 tree2 = do
    putStrLn "Saving..."
    writeFile wordsets $show tree1
    writeFile conversations $show tree2
    putStrLn "Done. Okay. Bye!"
      
userInput :: String -> String
userInput str = "u:" ++ str ++ "\n"


  

createWords :: String -> String
createWords str = undefined
          
printMemory :: IO() 
printMemory = do
    sets <- readFile wordsets
    sents <- readFile conversations
    putStrLn sets
    putStrLn sents
    
main = prog
  -- Compile this file using
  --
  --     ghc --make ExampleIO.hs
  --
  -- That produces an executable file named ExampleIO which will perform the
  -- instructions given by `prog`.
  --
  -- See also <http://www.cse.chalmers.se/edu/course/TDA555/FAQ.html#compilation>.