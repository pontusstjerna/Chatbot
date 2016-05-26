import Data.Char
import ChatEngine

memory :: String -> FilePath
memory str = "sentences.txt"

prog :: IO() 
prog = do 
    putStrLn "Loading... "
    inp <- readFile memory
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    chat inp

    
chat :: String -> IO String
chat = do
    putStr ":> "
    userInp <- getLine
    if(userInp) == "quit" then return ()
    else return answer $insert userInp 
      
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