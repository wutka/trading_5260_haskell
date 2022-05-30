{
module Parser where

import Lexer
import Resources

}

%name opParser Operation

%tokentype { Token }
%error { parseError }

%token
    transform { TokenTransform }
    transfer { TokenTransfer }
    inputs { TokenInputs }
    outputs { TokenOutputs }
    sym { TokenSymbol $$ }
    int { TokenInt $$ }
    '(' { TokenLParen }
    ')' { TokenRParen }
%%

Operation : '(' transform sym '(' inputs ResList ')' '(' outputs ResList ')' ')'
            { SpecTransform $ Transform $3 $6 $10 }
          | '(' transfer sym sym '(' ResList ')' { SpecTransfer $ Transfer $3 $4 $6 }

 ResList : '(' sym int ')' ResList { ResourceAmount $2 $3 : $5 }
         | { [] }
 
