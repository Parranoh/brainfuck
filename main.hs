import Prelude hiding (Either (..),init)
import qualified Prelude as P (Either (..),init)
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

input :: Register -> IO Register
input (l,_,r) = do
    eof <- IO.isEOF
    if eof
        then return (l,0,r)
        else do
            c <- getChar
            return (l,fromEnum c,r)

output :: Register -> IO ()
output (l,i,r) = putChar . toEnum $ i

exec :: Program -> Register -> IO Register
exec [] r = return r
exec (Inc:p) r = exec p $ inc r
exec (Dec:p) r = exec p $ dec r
exec (Left:p) r = exec p $ left r
exec (Right:p) r = exec p $ right r
exec (Input:p) r = input r >>= exec p
exec (Output:p) r = output r >> exec p r
exec p@(Loop l:p') r =
    if isZero r
        then exec p' r
        else do
            r' <- exec l r
            exec p r'


main :: IO ()
main = do
    files <- Env.getArgs
    M.when (length files /= 1) $ do
        putStrLn "Please provide exactly one file"
        Exit.exitFailure
    code <- readFile $ head files
    let prgm = runParser parseProgram . preprocess $ code
    maybe
        (putStrLn "Syntax error" >> Exit.exitFailure)
        (\(r,p) -> do
            if null r
                then exec p init >> return ()
                else putStrLn "Syntax error" >> Exit.exitFailure)
        prgm
