-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Quoptimize.CNF.Lexer
  ( Token(..)
  , TokenClass(..)
  , unlex
  , Alex(..)
  , runAlexDimacs
  , alexDimacsMonadScan
  , alexDimacsError
  ) where

import Control.Monad (liftM)
import System.FilePath (FilePath)
}

%wrapper "monadUserState"

$nonzero    = [1-9]
$decimal    = [0-9]
$space      = [\ \t\r]
@posint     = $nonzero $decimal*
@negint     = \- @posint

tokens :-
    <0>             $white+         ;
    <0>             p               { constLex TokenProb `andBegin` header }
    <0>             c               { begin comment }
    <0>             0               { constLex TokenZero `andBegin` clause }
    <0>             @negint         { charLex TokenNegNum `andBegin` clause }
    <0>             @posint         { charLex TokenPosNum `andBegin` clause }
    -- Logic for lexing header data (problem statement).
    <header>        $space+         ;
    <header>        cnf             { constLex TokenCNF }
    <header>        @posint         { charLex TokenSizeVal }
    <header>        \n              { begin 0 }
    -- Logic for lexing comments.
    <comment>       \n              { begin 0 }
    <comment>       .               ;
    -- Logic for lexing lines with atoms.
    <clause>        $space+         ;
    <clause>        0               { constLex TokenZero }
    <clause>        @negint         { charLex TokenNegNum }
    <clause>        @posint         { charLex TokenPosNum }
    <clause>        \n              { begin 0 }

{
-- File path is maintained to improve error messages.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The tokens returned by the parser.
data TokenClass = TokenProb
                | TokenCNF
                | TokenZero
                | TokenSizeVal String
                | TokenNegNum String
                | TokenPosNum String
                | TokenEOF
                deriving (Show)

data Token = Token AlexPosn TokenClass deriving (Show)

-- Converts tokens into strings for nicer error messages.
unlex :: TokenClass -> String
unlex TokenProb        = "p"
unlex TokenCNF         = "cnf"
unlex TokenZero        = "0"
unlex (TokenSizeVal n) = n
unlex (TokenNegNum n)  = n
unlex (TokenPosNum n)  = n
unlex TokenEOF         = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
    (p, _, _, _) <- alexGetInput
    return $ Token p TokenEOF

-- Now we must extract the strings and prepare the tokens.
charLex :: (String -> TokenClass) -> AlexAction Token
charLex f = \(p, _, _, s) i -> return $ Token p (f (take i s))

-- Inclusion for tokens that do not depend on input.
constLex :: TokenClass -> AlexAction Token
constLex = charLex . const

-- Error message generation.
alexDimacsError :: AlexPosn -> String -> Alex a
alexDimacsError (AlexPn _ l c) msg = do
    fp <- getFilePath
    alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- Improved error messages from the scanner.
alexDimacsMonadScan :: Alex Token
alexDimacsMonadScan = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError (p, _, _, s) ->
            alexDimacsError p ("lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip inp' len -> do
            alexSetInput inp'
            alexDimacsMonadScan
        AlexToken inp' len action -> do
            alexSetInput inp'
            action (ignorePendingBytes inp) len

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlexDimacs :: Alex a -> FilePath -> String -> Either String a
runAlexDimacs a fp input = runAlex input (setFilePath fp >> a)
}