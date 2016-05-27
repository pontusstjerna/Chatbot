import Data.Char
import ChatEngine
import AATree

memory :: FilePath
memory = "sentences.txt"

prog :: IO() 
prog = do 
    inp <- readFile memory
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    putStrLn "Loading..."
    chat $read inp
    
chat :: AATree (WordSet String) -> IO ()
chat tree = do
    putStrLn $show $size tree
    if tree /= emptyTree then putStrLn (constructNaive (Just (rootVal tree)) tree)
    else putStrLn "Created a file."
    putStr ":> "
    userInp <- getLine
    if userInp == "quit" then quit tree
    else chat (addSent userInp tree)
    
quit :: AATree (WordSet String) -> IO ()
quit tree = do
    putStrLn "Saving..."
    putStrLn "Done. Okay. Bye!"
    writeFile memory $show tree
      
userInput :: String -> String
userInput str = "u:" ++ str ++ "\n"


  

createWords :: String -> String
createWords str = undefined
          
printMemory :: IO() 
printMemory = do
    str <- readFile memory
    putStrLn str
    
main = prog
  -- Compile this file using
  --
  --     ghc --make ExampleIO.hs
  --
  -- That produces an executable file named ExampleIO which will perform the
  -- instructions given by `prog`.
  --
  -- See also <http://www.cse.chalmers.se/edu/course/TDA555/FAQ.html#compilation>.