import Prelude hiding (Either (..),init)
import qualified Prelude as P (Either (..),init)
import qualified Data.List as L
import qualified Control.Monad as M
import qualified Control.Applicative as A
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO

data Statement
    = Inc
    | Dec
    | Left
    | Right
    | Input
    | Output
    | Loop Program
    deriving Show

type Program = [Statement]

newtype Parser a = Parser
    { runParser :: String -> Maybe (String,a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser . fmap (fmap $ fmap f) $ p

instance Applicative Parser where
    pure x = Parser $ \s -> Just (s,x)
    Parser p <*> Parser q = Parser $ \s -> do
        (s',f) <- p s
        (s'',x) <- q s'
        pure (s'',f x)

instance Monad Parser where
    Parser mx >>= f = Parser $ \s -> do
        (s',x) <- mx s
        (runParser $ f x) s'

instance A.Alternative Parser where
    empty = Parser $ const Nothing
    Parser p <|> Parser q = Parser $ \s ->
        p s A.<|> q s

parseChar :: Char -> Parser Char
parseChar c = Parser f where
    f [] = Nothing
    f (x:xs) | x == c    = Just (xs,c)
             | otherwise = Nothing

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseInc :: Parser Statement
parseInc = const Inc <$> parseChar '+'

parseDec :: Parser Statement
parseDec = const Dec <$> parseChar '-'

parseLeft :: Parser Statement
parseLeft = const Left <$> parseChar '<'

parseRight :: Parser Statement
parseRight = const Right <$> parseChar '>'

parseInput :: Parser Statement
parseInput = const Input <$> parseChar ','

parseOutput :: Parser Statement
parseOutput = const Output <$> parseChar '.'

parseLoop :: Parser Statement
parseLoop = fmap Loop $ parseChar '[' *> parseProgram <* parseChar ']'

parseStatement :: Parser Statement
parseStatement
    =     parseInc
    A.<|> parseDec
    A.<|> parseLeft
    A.<|> parseRight
    A.<|> parseInput
    A.<|> parseOutput
    A.<|> parseLoop

parseProgram :: Parser Program
parseProgram = A.many parseStatement

preprocess :: String -> String
preprocess = filter (flip elem "+-<>,.[]")


type Register = ([Int],Int,[Int])

data EofBehavior = Zero | MinusOne | LeaveUnchanged

init :: Register
init = ([],0,[])

isZero :: Register -> Bool
isZero (_,0,_) = True
isZero _       = False

inc :: Register -> Register
inc (l,i,r) = (l,(i + 1) `mod` 256,r)

dec :: Register -> Register
dec (l,i,r) = (l,(i - 1) `mod` 256,r)

left :: Register -> Register
left ([],i,r) = ([],0,i:r)
left (l:ls,i,r) = (ls,l,i:r)

right :: Register -> Register
right (l,i,[]) = (i:l,0,[])
right (l,i,r:rs) = (i:l,r,rs)

input :: EofBehavior -> Register -> IO Register
input eofBehavior reg@(l,i,r) = do
    eof <- IO.isEOF
    if eof
        then return $ case eofBehavior of
            MinusOne -> (l,255,r)
            Zero -> (l,0,r)
            LeaveUnchanged -> reg
        else do
            c <- getChar
            let i = fromEnum c `mod` 255
            return (l,i,r)

output :: Register -> IO ()
output (l,i,r) = putChar . toEnum $ i

exec :: EofBehavior -> Program -> Register -> IO Register
exec eof = go where
    go [] r = return r
    go (Inc      : p)  r = go p $ inc r
    go (Dec      : p)  r = go p $ dec r
    go (Left     : p)  r = go p $ left r
    go (Right    : p)  r = go p $ right r
    go (Input    : p)  r = input eof r >>= go p
    go (Output   : p)  r = output r >> go p r
    go p@(Loop l : p') r =
        if isZero r
            then go p' r
            else do
                r' <- go l r
                go p r'


main :: IO ()
main = do
    argv <- Env.getArgs
    let (flags,files) = L.partition ((==) '-' . head) argv
    M.when ("-b" `elem` flags) $ IO.hSetBuffering IO.stdin IO.NoBuffering
    M.when (length files /= 1) $ do
        putStrLn "Please provide exactly one file"
        Exit.exitFailure
    code <- readFile $ head files
    let prgm = runParser parseProgram . preprocess $ code
    maybe
        (putStrLn "Syntax error" >> Exit.exitFailure)
        (\(r,p) ->
            if null r
                then exec (eofBehavior flags) p init >> return ()
                else putStrLn "Syntax error" >> Exit.exitFailure)
        prgm
    where
        eofBehavior flags
            | "-n" `elem` flags = LeaveUnchanged
            | "-1" `elem` flags = MinusOne
            | otherwise         = Zero
