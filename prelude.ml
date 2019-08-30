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

(***********************************************************************)
(* This module summerize the static information of the Store.         **)
(***********************************************************************)
open Javasyntax
open Display

exception Class_Not_Found 			of string * string
exception Not_Applicable 				of string * string

type t = { mutable c: compilunit list} 

let data = { c =
	       [ CompilC(
		   Package("java.lang"),
		   [],
		   Class("Object",Extends(""),[],
			 [ ConstrMem(CId("Object",[]),
				     BlockIt([(**<<>>**)],
					     Stack_of_env.createEnv()),
				     []);
			   MethMem(MId("equals",RetType(PrimType(BoolType)),[ClType("Object")]),
				   BlockIt([(**<<>>**)],Stack_of_env.createEnv()),
				   ["obj"]);
			   MethMem(MId("notify",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("notifyAll",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("wait",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("finalize",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			 ]
			)
		 );
		 CompilC(
		   Package("java.lang"),
		   [ Import("java.security.AccessController");Import("java.security.AccessControllerContext");
		     Import("java.util.Map");Import("java.util.Collections");
		   ],
		   Class("Thread",Extends("Object"),[],
			 [ ConstrMem(CId("Thread",[]),
				     BlockIt([(**<<>>**)],Stack_of_env.createEnv()),
				     []);
			   MethMem(MId("currentThread",RetType(ClType("Thread")),[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("yield",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("sleep",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("start",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("run",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("exit",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("stop",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("interrupt",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("interrupted",RetType(PrimType(BoolType)),[]),
				   BlockIt([(**<<>>**)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("isAlive",RetType(PrimType(BoolType)),[]),
				   BlockIt([(**<<>>**)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("susptend",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("resume",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			   MethMem(MId("join",Void,[]),
				   BlockIt([(**<<>>*)],Stack_of_env.createEnv()),
				   []);
			 ]
			)			
		 );				
	       ]
	   } 

(** returns de compilation unit corresponding to the class 'c'. **)
let getCmpUn c =
  try
    Some (List.find (function CompilC(_,_,Class(cl,_,_,_)) -> if cl=c then true else false | _ -> true) data.c)
  with 
      Not_found -> None
	
(** returns the list of classes implemented by 'c' **)
let getImp c =
  match getCmpUn c with
    | Some CompilC(_,_,Class(_,_,impls,_)) -> if impls=[] then None else Some impls
    | None -> raise (Class_Not_Found("<getImpl>",(Display.disp_classtype ":" c Plenty)))
    | _	 -> raise (Not_Applicable("<getImpl,getCmpUn>",(Display.disp_classtype ":" c Plenty)))

(** returns the class which 'c' extends **)
let getSuper c =
  match getCmpUn c with
    | Some CompilC(_,_,Class(cl,Extends(cc),_,_)) -> if cc="" then None else Some cc
    | None -> raise (Class_Not_Found("<getSuper>",(Display.disp_classtype ":" c Plenty)))
    | _	 -> raise (Not_Applicable("<getSuper,getCmpUn>",(Display.disp_classtype ":" c Plenty)))

(** returns the list of method declarations of the class 'c'. **)
let methoddecl c =
  match getCmpUn c with
    | Some CompilC(_,_,Class(_,_,_,cml)) -> 
	(List.fold_left (function a -> function MethMem(md) -> a@[md] | _ -> a) [] cml)
    | None -> raise (Class_Not_Found("<methoddecl>",(Display.disp_classtype ":" c Plenty)))
    | _	 -> raise (Not_Applicable("<methoddecl,getCmpUn>",(Display.disp_classtype ":" c Plenty)))
	
(** returns the list of constructor declarations of the class 'c'. **)
let constructordecl c =
  match getCmpUn c with
    | Some CompilC(_,_,Class(_,_,_,cml)) -> 
	(List.fold_left (function a -> function ConstrMem(cd) -> a@[cd] | _ -> a) [] cml)
    | None -> raise (Class_Not_Found("<constructordecl>",(Display.disp_classtype ":" c Plenty)))
    | _	 -> raise (Not_Applicable("<constructordecl,getCmpUn>",(Display.disp_classtype ":" c Plenty)))
	
(** returns the list of field declarations of the class 'c'. **)
let fielddecl c =
  match getCmpUn c with
    | Some CompilC(_,_,Class(_,_,_,cml)) -> 
	(List.fold_left (function a -> function FldMem(fd) -> a@[fd] | _ -> a) [] cml)
    | None -> raise (Class_Not_Found("<fielddecl>",(Display.disp_classtype ":" c Plenty)))
    | _	-> raise (Not_Applicable("<fielddecl,getCmpUn>",(Display.disp_classtype ":" c Plenty)))

(** loads the compilation unit 'cu' in the store 's' **)
let load cu =
  data.c <- cu::data.c
;;

