{
module Parser where

import Lexer
import Resources

}

%name opParser Operation
%name csvLineParser CsvLine

%tokentype { Token }
%error { parseError }

%token
    transform { TokenTransform }
    transfer { TokenTransfer }
    inputs { TokenInputs }
    outputs { TokenOutputs }
    sym { TokenSymbol $$ }
    str { TokenString $$ }
    int { TokenInt $$ }
    '(' { TokenLParen }
    ')' { TokenRParen }
    ',' { TokenComma }
%%

Operation : '(' transform sym '(' inputs ResList ')' '(' outputs ResList ')' ')'
            { OpTransform $ Transform $3 $6 $10 }
          | '(' transfer sym sym '(' ResList ')' { OpTransfer $ Transfer $3 $4 $6 }

 ResList : '(' sym int ')' ResList { ResourceAmount $2 $3 : $5 }
         | { [] }

CsvLine : CsvLineRev { reverse $1 }

CsvLineRev : CsvLineRev ',' str { CsvString $3 : $1 }
           | CsvLineRev ',' int { CsvInt $3 : $1 }
           | str { [CsvString $1] }
           | int { [CsvInt $1] }
