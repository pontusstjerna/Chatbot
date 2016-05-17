memory :: FilePath
memory = "memory.txt"

prog :: IO() 
prog = do 
    putStrLn "Hello! My name is Lillebil. Talk to me!"
    remember
    putStrLn "Noticed."

listen :: IO()
listen = do
    userInp <- getLine
    appendFile memory userInp
    
remember :: IO()
remember = do
          appendFile memory "u:"
          listen
          appendFile memory "\n"
    
    
main = prog
  -- Compile this file using
  --
  --     ghc --make ExampleIO.hs
  --
  -- That produces an executable file named ExampleIO which will perform the
  -- instructions given by `prog`.
  --
  -- See also <http://www.cse.chalmers.se/edu/course/TDA555/FAQ.html#compilation>.