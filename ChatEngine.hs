module ChatEngine
where

data Conversation a = Conv a [(Int, String)]
    deriving (Eq, Show, Read)

