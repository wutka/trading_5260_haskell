{
module Parser where

import Lexer
import Resources

}

%name opParser Operation
%name csvLineParser CsvLine
%name updateParser UpdateLine
%name configLineParser ConfigLine

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
    double { TokenDouble $$ }
    '(' { TokenLParen }
    ')' { TokenRParen }
    ',' { TokenComma }
    '*' { TokenStar }
    '/' { TokenSlash }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '=' { TokenEqual }
    NE { TokenNotEqual }
    '>' { TokenGreater }
    GE { TokenGreaterEqual }
    '<' { TokenLess }
    LE {TokenLessEqual }
    AND { TokenAnd }
    OR { TokenOr }
    NOT { TokenNot }
    computedField { TokenComputedField }
    updatedField { TokenUpdatedField }
    threshold { TokenThreshold }
%%

Operation : '(' transform sym '(' inputs ResList ')' '(' outputs ResList ')' ')'
            { OpTransform $ Transform $3 $6 $10 }
          | '(' transfer sym sym '(' ResList ')' { OpTransfer $ Transfer $3 $4 $6 }

 ResList : '(' sym int ')' ResList { ResourceAmount $2 $3 : $5 }
         | { [] }

CsvLine : CsvLineRev { reverse $1 }

CsvLineRev : CsvLineRev ',' str { CsvString $3 : $1 }
           | CsvLineRev ',' sym { CsvString $3 : $1 }
           | CsvLineRev ',' int { CsvInt $3 : $1 }
           | CsvLineRev ',' double { CsvDouble $3 : $1 }
           | str { [CsvString $1] }
           | sym { [CsvString $1] }
           | int { [CsvInt $1] }
           | double { [CsvDouble $1] }

ConfigLine : sym '=' str { ($1, CsvString $3) }
    	   | sym '=' sym { ($1, CsvString $3) }
           | sym '=' int { ($1, CsvInt $3) }
           | sym '=' double { ($1, CsvDouble $3) }
      
ExpressionPart : sym { REFieldRef $1 }
             | int { REConstant (fromIntegral $1) }
             | double { REConstant $1 }
             | '(' Expression ')' { $2 }

ExMulDivPart : ExMulDivPart '*' ExMulDivPart { REMultiply $1 $3 }
             | ExMulDivPart '/' ExMulDivPart { REDivide $1 $3 }
             | ExpressionPart { $1 }

Expression : Expression '+' Expression { REAdd $1 $3 }
             | Expression '-' Expression { RESubtract $1 $3 }
             | ExMulDivPart { $1 }

CompBase : Expression '>' Expression { RCGreater $1 $3 }
         | Expression GE Expression { RCGreaterEqual $1 $3 }
         | Expression '<' Expression { RCLess $1 $3 }
         | Expression LE Expression { RCLessEqual $1 $3 }
         | Expression '=' Expression { RCEqual $1 $3 }
         | Expression NE Expression { RCNotEqual $1 $3 }

Comparison : Comparison AND Comparison { RCAnd $1 $3 }
           | Comparison OR Comparison { RCOr $1 $3 }
           | NOT Comparison { RCNot $2 }
           | CompBase { $1 }

UpdateLine : updatedField ',' str ',' sym ',' Expression { RUUpdatedField $3 $5 $7 }
           | threshold ',' str ',' Comparison ',' sym ',' Expression { RUThreshold $3 $5 $7 $9 }
