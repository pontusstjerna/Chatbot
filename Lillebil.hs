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
    wordsets <- readFile wordsets
    conversations <- readFile conversations
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    putStrLn "Loading..."
    chat (read wordsets) (read conversations)
    
chat :: AATree (WordSet String) -> AATree (WordSet String) -> IO ()
chat tree1 tree2 = do
    putStrLn $show $size tree1
    if tree1 /= emptyTree then putStrLn (constructNaive (Just (rootVal tree1)) tree1)
    else putStrLn "Created a file."
    putStr ":> "
    userInp <- getLine
    putStrLn $show $bestSentence userInp $map word $inorder tree2
    if userInp == "quit" then quit tree1 tree2
    else chat (addSent userInp tree1) tree2
    
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