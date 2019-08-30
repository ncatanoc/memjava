(***********************************************************************)
(*                                                                     *)
(*                           Event Spaces                              *)
(*                                                                     *)
(*                  Néstor  CATAÑO, Lemme project                      *)
(*                     INRIA Sophia Antipolis                          *)
(*                 2004 route des Lucioles-B.P. 93                     *)
(*              06902 Sophia Antipolis Cedex (France)                  *)
(*                                                                     *)
(***********************************************************************)
type identifier = string

type literal =
  | IntLit of int
  | BoolLit of bool
  | Null

type classtype = identifier

type primitivetype =
  | BoolType
  | IntType

type jtype =
  | PrimType of primitivetype 
  | ClType of classtype

type returntype =
  | RetType of jtype 
  | Void

(** Identifiers come equipped with some types. Fields identifiers
  (field access representation) carry the class type of the 
  declarations since for fields
  identifiers there is a statically type resolved overloading.
  A field identifier is composed by the name of the field and its
  static type, e.g, given the declaration 'A a = new B()' in class
  'C', the field 'a' will be represented as 'FId a A'.
**)
type fieldidentifier = FId of identifier * jtype 

(** Method indentifiers have to be resolved by dynamic lookup, 
  hence method identifiers carry the static type of invocation
  needed for dynamic dispatch. 
  The declaration of the method 'm' in class 'C' will be
  represented  as 'MId(m,t,lT)', where 't' and 'lT' are the
  return type and the list of type of the parameters.
  method. 
**)
(** Method identifier **)
type methid = 
    MId of identifier * returntype * (jtype list)

(** Constructor identifier **)
type constrid = 
    CId of identifier * (jtype list)

type mcallid =
  | MCId of methid 
  | CCId of constrid

(** Reference to an object in the Store. **)
type nonnullobj = int

(** Reference to objects **)
type obj =
  | Nonnull of nonnullobj
  | Nullobj 

(** Right values are references to the Store (locations) or values. **)
type rval =
  | Oref of obj	
  | Ival of int	
  | Bval of bool

type throws = Throw of obj
 
type thread = obj

type identthis =
  | Ident of identifier 
  | ThisExp

type envmapping = identthis -> (rval option)

module IdentThis = Set.Make(struct type t = identthis
				   let compare = function idt1 -> function idt2 ->
				     match idt1,idt2 with
				       | (Ident(id1),Ident(id2)) -> compare id1 id2
				       | (ThisExp,ThisExp) -> 0
				       | (Ident(id1),ThisExp) -> 1
				       | (ThisExp,Ident(id2)) -> -1
			    end
										);;

type env = Env of (IdentThis.t) * envmapping

(**
 Class D{
  boolean b = true ;
 }

 Class C {
  void m(){
   int i = 5;
   D d = new D();
   (HERE) <==========
  }
  public static void main(String args[]){
   C c = new C();
   c.m();
  }
 }

 Env([this;i;d],
   [ (ThisExp,Oref(0)),
     (Ident("i"),Ival(5)),
     (Ident("d"),Oref(1))]
**)

(** Left values,i.e, representations for field access **)
(** Lval(1,FId("b",PrimType(BoolType))) **)
type lval = Lval of nonnullobj * fieldidentifier

type event =
  | Lock of thread * obj		
  | Unlock of thread * obj 	
  | Use	of thread * lval * rval		
  | Assign of thread * lval * rval 	
  | Load of thread * lval * rval 	
  | Store of thread * lval * rval 	
  | Read of thread * lval * rval 	
  | Write of thread * lval * rval

type unaryop = 	rval -> rval
and binaryop = 	rval -> rval -> rval

(* type modifier =
  | Public | Protected | Private 	
  | Static | Abstract  | Final 		
  | Native | Synchronized | Transient 
  | Volatile
*)

type block = BlockIt of (stat list) * env

and stat =
  | Nop	| SemiCol 
  | BlockStmt of block 				
  | ExpStmt of stmtexpr 		
  | SyncStmt of expr * block 
  | IfStmt of expr * stat 	
  | WhileStmt of expr * stat 	
  | ThrowStmt of expr 
  | TryStmt of block * catch * (catch list) 
  | TryFinStmt of block * (catch list) * block 
  | ReturnStmt of (expr option) 
  | VarDeclStmtN of jtype * identifier 
  | VarDeclStmtE of jtype * identifier * expr
		
and stmtexpr =
  | Ass	of lefthandside * expr 
  | NewC of classtype * expr list 
  | MCall of expr * mcallid * expr list 	
  | AFrame of mcallid * block 						
  | ValS of rval 	
  | ThrowSE of throws 
  | ReturnSE of (rval option) 

and expr =
  | Lit	of literal 			
  | Acc	of lefthandside	
  | This
  | InstOf of expr * classtype 
  | UnOp of unaryop * expr 	
  | BinOp of expr * binaryop * expr 
  | StmtExp of stmtexpr
	
and catch = CC of jtype * identifier * block

and lefthandside =
  | Var	of identifier 
  | Field of expr * fieldidentifier

type aterm =
  | ExprT of expr 				
  | ExprSeqT of (expr list) 
  | StatT of stat 				
  | StatSeqT of (stat list)
	
type staterecord =
  | R 
  | W of obj * int 
  | N of obj * int 
  | D

type package = Package of identifier

type import = Import of identifier

type extends = Extends of identifier
	
type implements = Implements of identifier

(** Constructors and method declarations are cuadruples
  of method-constructor signatures, the modifier, the body and
  its formal parameters.
**)
type methoddecl = methid * block * (identifier list)
type constrdecl = constrid * block * (identifier list)
type fielddecl	= jtype	* identifier

type classmember =
  | FldMem of fielddecl		
  | MethMem of methoddecl 	
  | ConstrMem of constrdecl

(** a class declaration is composed by a quadruple of the name
  of the class, the extended class, the implemented classes
  and a list of members of the class.
**)
type classdecl = Class of identifier * extends * (implements list) * classmember list

type interfacedecl = Interface of identifier * extends
	
type compilunit =
  | CompilC of package * (import list) * classdecl	
  | CompilI of package * (import list) * interfacedecl 

(***********************************************************************)
(**************** Some functionalities on the Java Syntax **************)
(***********************************************************************)

(** establishes whether its parameter is defined or not.
  A parameter is defined when it can be seen as Some _.
  isDef: 'a option -> bool
**)
let isDef = function Some _ -> true | None -> false

(** converts a lefthandside in a lval 
  lhsLval: lefthandside -> lval option
**)
let lhsLval = function Field(StmtExp(ValS(Oref(Nonnull(o)))),f) -> Some (Lval(o,f)) | _ -> None
		
(** establishes whether 'lhs' is a left value 
  isLval: lefthandside -> bool
**)
let isLval lhs = isDef (lhsLval(lhs))
		
(** converts list of expressions into a list of right values 
  expRvalList: expr list -> (rval list) option
**)
let rec expRvalList = function
| [] -> Some []
| StmtExp(ValS(v))::es ->
	begin	match expRvalList es with
	| None -> None
	| Some rl -> Some (v::rl)
	end
| _ -> None	

(** converts a list of right values into a list of right values. 
  rvalExprList: rval list -> (expr list)
**)
let rec rvalExprList = function
| [] -> []
| v::vs -> (StmtExp(ValS(v)))::(rvalExprList vs)

(** As a result of a invocation of a 'stop' of some 'Thread' 'th',
    this is queued in the die queue (see rule 'stop1'). An exception is
    then thrown up by 'stop2' as deep inside the structure of the
    program as necessary to allow a catch by a possibly enclosing
    'try-catch' statement. This is ensured by the side condition that
    the term is a redex. As ThrowSE(v) and ReturnSE[v] are not contained
    in this list of redices, a thread cannot stop as long as it is
    performing a transfer of control, i.e., performing pop-out rules.
    isRedex: aterm -> bool **)
let isRedex = function
  | ExprT(StmtExp(Ass(Var(_),StmtExp(ValS(_)))))
  | ExprT(StmtExp(Ass(Field(StmtExp(ValS(Oref(Nonnull(_)))),_),StmtExp(ValS(_)))))
  | ExprT(Acc(Field(StmtExp(ValS(Oref(Nullobj))),_)))
  | ExprT(StmtExp(Ass(Field(StmtExp(ValS(Oref(Nullobj))),_),_)))
  | ExprT(Acc(Field(StmtExp(ValS(Oref(Nonnull(_)))),_)))
  | ExprT(This)
  | StatT(ExpStmt(NewC(_)))
  | ExprT(Lit(_))
  | ExprT(UnOp(_,StmtExp(ValS(_))))
  | ExprT(BinOp(StmtExp(ValS(_)),_,StmtExp(ValS(_))))
  (** | StatT(VarDeclStmtE(_,_,StmtExp(ValS(_)))) **)
  | StatT(ExpStmt(ValS(_)))
  | StatT(SemiCol)
  | StatT(IfStmt(StmtExp(ValS(_)),_))
  | StatT(BlockStmt(BlockIt([],_)))
  | ExprT(StmtExp(AFrame(_,BlockIt([],_))))
  | StatT(ThrowStmt((StmtExp(ValS(Oref(_))))))
  | StatT(ReturnStmt(Some StmtExp(ValS(_))))
  | StatT(ReturnStmt(None))
  | StatT(TryStmt(BlockIt([],_),CC(_,_,_),_))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("start",Void,[])),[])))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("stop",Void,[])),[])))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("wait",Void,[])),[])))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("notify",Void,[])),[])))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("notifyAll",Void,[])),[])))
  | StatT(ExpStmt(MCall(StmtExp(ValS(Oref(_))),MCId(MId("isAlive",Void,[])),[]))) ->
      true
      
  | ExprT(StmtExp(MCall(StmtExp(ValS(Oref(p))),m,eE))) ->
      begin match expRvalList eE with
	| Some vl -> true 
	| None -> false
      end

  | _ -> false

;;
