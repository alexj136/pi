{
module Parser where

import Util
import Lexer
import Syntax
}

%name parse P

%tokentype { Token }
%error { parseError }

%left Bar
%left Dot

%token
    At   { TkAt      }
    Dot  { TkDot     }
    Bar  { TkBar     }
    FSl  { TkFSl     }
    Bang { TkBang    }
    LPar { TkLPar    }
    RPar { TkRPar    }
    Zero { TkZero    }
    Name { TkName $$ }
%%

P :: { PiTerm }
P : P Bar P                   { Par $1 $3    }
  | Name FSl Name Dot P       { Out $1 $3 $5 }
  | Name LPar Name RPar Dot P { In $1 $3 $6  }
  | Bang P                    { Rep $2       }
  | LPar At Name RPar P       { New $3 $5    }
  | Zero                      { O            }
  | Name FSl Name             { Out $1 $3 O  }
  | Name LPar Name RPar       { In $1 $3 O   }
  | LPar P RPar               { $2           }

{
parseError :: [Token] -> a
parseError [] = error "Reached end of input while parsing"
parseError ts = error $ "Parse error: " ++ show ts
}
