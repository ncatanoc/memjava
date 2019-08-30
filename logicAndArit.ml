open Javasyntax;;
open Display ;;


type expreArit = 
  | Constante        of int
  | Suma             of expreArit * expreArit
  | Resta            of expreArit * expreArit
  | Division         of expreArit * expreArit
  | Multiplicacion   of expreArit * expreArit;; 


type expreLogic =
  | Literal2  of literal  
  | And      of expreLogic * expreLogic 
  | Or       of expreLogic * expreLogic  
  | Not      of expreLogic  ;;
  
type relOp =
  | Mayor        of relOp * relOp 
  | MayorIgual   of relOp * relOp 
  | Menor        of relOp * relOp 
  | MenorIgual   of relOp * relOp 
  | Igual        of relOp * relOp 
  | Diferente    of relOp * relOp ;;
    
let rec moduloAritmetico operando = 
match operando with 
 Constante(t1)-> t1
  |  Suma(t1,t2)           -> (moduloAritmetico t1) + (moduloAritmetico t2)
  |  Resta(t1,t2)          -> (moduloAritmetico t1) - (moduloAritmetico t2)
  |  Division(t1,t2)       -> (moduloAritmetico t1) / (moduloAritmetico t2)
  |  Multiplicacion(t1,t2) -> (moduloAritmetico t1) * (moduloAritmetico t2);;


let relOpEval r =
match r with
  | Mayor(t1,t2)      -> t1 > t2
  | MayorIgual(t1,t2) -> t1 >= t2
  | Menor(t1,t2)      -> t1 < t2
  | MenorIgual(t1,t2) -> t1 <= t2
  | Igual(t1,t2)      -> t1 == t2 
  | Diferente(t1,t2)  -> t1 != t2;;


relOpEval (Mayor(Literal2(IntLit(20)) , Literal2(IntLit(15))));;


(**
relOpEval(MayorIgual(LiIntLit(20)) , Literal2(IntLit(15))));;
relOpEval(Menor(Literal2(IntLit(20)) , Literal2(IntLit(15)) )) ;;
relOpEval(MenorIgual(Literal2(IntLit(20)) , Literal2(IntLit(20)))) ;;
relOpEval(Igual(Literal2(IntLit(30)) , Literal2(IntLit(30))));;
relOpEval(Diferente(Literal2(IntLit(20)) , Literal2(IntLit(15)))) ;;
**)
