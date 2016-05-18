import Data.Char

memory :: String -> FilePath
memory str = "sentences.txt"

prog :: IO() 
prog = do 
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    chat

    
chat :: IO()
chat = do
    putStr ":> "
    run <- remember
    if run then do
      putStrLn "Noticed. Answers not yet implemented."
      chat
    else return ()

listen :: IO String
listen = do
    userInp <- getLine
    return userInp
    
remember :: IO Bool
remember = do
          str <- listen
          if str /= "quit"
          then do 
           appendFile memory (userInput str) 
           appendFile memory (createWords str)
           return True
          else return False
          
userInput :: String -> String
userInput str = "u:" ++ str ++ "\n"

createWords :: String -> String
createWords str = undefined
          
printWords :: IO() 
printWords = do
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