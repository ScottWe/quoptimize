-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Quoptimize.CNF.Parser (parseDimacs) where

import Data.Maybe (fromJust)
import Quoptimize.CNF.Lexer
import Quoptimize.CNF.Language
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
    p               { Token _ TokenProb }
    cnf             { Token _ TokenCNF }
    zero            { Token _ TokenZero }
    sizeval         { Token _ (TokenSizeVal $$) }
    negint          { Token _ (TokenNegNum $$) }
    posint          { Token _ (TokenPosNum $$) }

%%

Program : p cnf Size Size CnfBody     { DimacsCNF $3 $4 $5 }
        | CnfBody                     { DimacsCNFMin $1 }

Size    : sizeval                     { parsePosInt $1 }

CnfBody : Clause                      { [$1] }
        | Clause CnfBody              { $1 : $2 }

CnfAtom : posint                      { parsePosCnfAtom $1 }
        | negint                      { parseNegCnfAtom $1 }

Clause : CnfAtom zero                 { [$1] }
       | CnfAtom Clause               { $1 : $2 }

{
parsePosInt :: String -> PosInt
parsePosInt x = fromJust $ toPosInt $ read x

parsePosCnfAtom :: String -> DimacsAtom
parsePosCnfAtom x = PosLit $ parsePosInt x

parseNegCnfAtom :: String -> DimacsAtom
parseNegCnfAtom x = NegLit $ fromJust $ toNegInt $ read x

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexDimacsMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexDimacsError p msg
    where msg = "parse error at token '" ++ unlex t ++ "'"

parseDimacs :: FilePath -> String -> Either String DimacsFile
parseDimacs fp input = runAlexDimacs parse fp input
}