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
(*            This module implements a Java store.                    **)
(* The objects are permanent entities that survive the blocks in      **)
(* which they are created; therefore the collection of their instance **)
(* variables (containing the values of their attributes) is kept in a **)
(* separated structure: the store.                                    **)
(***********************************************************************)

(***********************************************************************)
(** A store 's' is composed by a triple <o,id,env[id]>, where 'o' is **)
(** an object location, 'id' is the name of class where 'o' appears  **)
(** and 'env[id]' is the environment of 'id'.                        **)
(***********************************************************************)
open Javasyntax
open Display

module SetStore = Set.Make( 
  struct
    type t = int * identifier * env
    let compare (i1,_,_) (i2,_,_) = compare i1 i2
  end
);;

type t = SetStore.t
    
(** The library 'Set' does not have a 'find' method. I provide an
  implmetation of that method.  **)
let find s o =
  try match o with
    | Nonnull(p) -> Some (SetStore.choose ( SetStore.filter 
					      (function (pp,_,_) -> if pp=p then true else false)
					      s
					  ))
    | Nullobj -> None
  with
      Not_found -> None
	
type disp = {mutable prf: string; mutable res: string}

exception Class_Not_Found 			of string * string
exception Object_Not_Found			of string * string
exception Option_Not_Considered of string * string
exception Not_Applicable 				of string * string

(** display the store 's' **) 
let display s pfix swt = 
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"STORE" in
  let () = ( SetStore.fold
	       (function (i,id,envr)-> function a ->
		  r.res <- r.res ^(disp_store_element r.prf i id envr swt)
	       )		
	       s
	       ()	
	   )
  in r.res 


(* constructs an empty store. *)
let create = SetStore.empty

(* converts a literal into a right value *)
let value = function
  | IntLit(l) -> Ival(l)
  | BoolLit(l) -> Bval(l)
  | Null -> Oref(Nullobj)

(** returns the default value of a variable of type 'jt'.
  JSL-4.5.5 
**)
let init_val = function
  | PrimType(BoolType)	-> Bval(false)
  | PrimType(IntType) -> Ival(0)
  | ClType(_) -> Oref(Nullobj)

(* returns the class of the object 'o' in the store 's' *)
let getClass s o =
  match find s o with 
    | Some (_,id,_) -> Some id
    | None -> None
	
(* returns the environment of 'p' in the store 's' *)
let getEnvironment s o =
  match find s o with
    | Some (_,_,envr) -> Some envr
    | None -> None

(** returns the Right value corresponding to the left value 'l'
		in the store 's'. **)
let getRval s = function
  | Lval(p,FId(id,_)) ->
      begin match find s (Nonnull(p)) with
	| Some (_,_,envrm) -> Stack_of_env.lookUp envrm (Ident(id))	
	| None -> None
      end

(** update 'l=Lval(p,FId(id,_))' to 'r' in the store 's', if 'id' exists previously;
  add '(l,r)' to 's', otherwise.
 **)
let update s l r =
  match l with
    | Lval(p,FId(id,_)) ->
	begin match find s (Nonnull(p)) with
	  | Some (_,c,envrm) -> let envrm2 = Stack_of_env.update envrm (Ident(id)) r in
	    let s2 = SetStore.remove (p,c,envrm) s in
	    let s3 = SetStore.add (p,c,envrm2) s2 in
	      s3
	  | None -> raise (Object_Not_Found("<update,find>",
					    (disp_nonnullobj ":" p Plenty)))
	end						

(** Like 'methodbody' but only searches in the class 'c'.
  This method returns None if 'm' is not found in 'c'.
**)
let methodbodyC c m =
  let ml = Prelude.methoddecl c in
    begin 
      try
	begin match (List.find (function (mm,_,_) -> mm=m) ml) with
	  | (_,bl,il) -> Some (bl,il)
	end
      with
	  Not_found -> None
    end

(** Like 'methodbodyC' **)
let constructorbodyC c m =
  match m with
    | CId(id,jtl) ->
	let ml = Prelude.constructordecl c in
	  begin 
	    try
	      begin match (List.find (function (mm,_,_) -> mm=m) ml) with
		| (_,BlockIt(ss,envrm),il) -> 
		    (** initialize the values of the field of class 'c' to
		      the values of its declarations.
		    **)
		    let vdsl =( List.fold_left
				  ( function a -> function 
					(jt,id) -> a@[VarDeclStmtN(jt,id)]
				  )
				  []
				  (Prelude.fielddecl c)
			      ) in
		      begin match ss with
			| s1::sS ->
			    begin match s1 with
			      | ExpStmt(MCall(This,CCId(CId("this",_)),_))
			      | ExpStmt(MCall(This,CCId(CId("super",_)),_)) ->
				  Some (BlockIt(ss,envrm),il)
				  
			      | _ -> (** add super(); **)
				  Some (BlockIt((ExpStmt(MCall(This,CCId(CId("super",[])),[])))::ss,envrm),il)
			    end
			| [] ->
			    Some (BlockIt([ExpStmt(MCall(This,CCId(CId("super",[])),[]))],envrm),il)
		      end
	      end
	    with
		Not_found ->
		  if jtl=[] then (** c(){} **)
		    Some (BlockIt([],Stack_of_env.createEnv()),[])
		  else None	
	  end
	
(** It performs the run-time retrieval of methods, where 's' is the store
		containing the class and method declarations,'c' is the class of the 
		object for which the method is beeing invoked and 'm'=(id,t,lT)
		is a method identifier containing the identifier of the method,
		the return type and the list of type of formal parameters.
		It returns the body of 'm' (a statment list ) which has signature lT->t
		and also returns the list of its formal parameters. This method
		inspectes the classes	between 'c' and 'Object'.
**)
let rec methodbody c m =
  let f cc = (** implementing classes **)
    begin match Prelude.getImp cc with
      | None -> None
      | Some ccil -> 
	  ( List.fold_left 
	      ( function b -> function Implements(cci) ->
		  if b=None then
		    if cci="" then None else
		      begin match methodbodyC cci m with
			| None -> (methodbody cci m)
			| Some mb -> Some mb 
		      end
		  else b
	      ) 
	      None 
	      ccil
	  )								
    end
  and g cc = (** extending classes **)
    begin match Prelude.getSuper cc with
      | None -> None
      | Some cce ->
	  if cce="" then None else
	    begin match methodbodyC cce m with
	      | None -> methodbody cce m (** class java.lang.Object **)
	      | Some mb -> Some mb
	    end
    end
  in begin match methodbodyC c m with
    | Some mb -> Some mb
    | None ->
	(** search in 'extending' and 'implementing' classes of 'c' **)
	begin match g c with 
	  | None -> f c 
	  | Some mb -> Some mb
	end
    end
	

(** Like methodbody **)
let rec constructorbody c m  = constructorbodyC c m


(** It produces an Activation Frame at run-time.'s' represents the store,
  'o' the object on which the method 'm' with parameters 'vl' is called.
  It retrieves the method (or constructor) 'm' with the aid of 'methodbody'
  and then replaces in the execution environment the formal parameters
  by the actual parameters.	
**)
let frame s o m vl =
  (** JSL-15.12.1 compile time step 1: 
    determine the class or interface to search **)
  begin match (getClass s o),(getEnvironment s o) with
    | Some c, Some envr	-> (*-* 'envr' contains the values for the fields of 'o' *-*) 
	(** JSL-15.12.1 compile time step 2: 
	  determine the method signature **)
	let mb = begin match m with MCId(mid) -> methodbody c mid | CCId(cid) -> constructorbody c cid end in	
	  begin	match mb with
	    | None -> None
	    | Some (BlockIt(sl,_),idl) ->
		Some (m,
		      BlockIt(sl,Stack_of_env.updatelist (Stack_of_env.update envr ThisExp (Oref(o))) 	(*-* this\o *-*) 
				(List.map (function i -> Ident(i)) idl) vl))	(*-* I\V *-*)
	  end
    | _,_ -> None	
  end

(**
	class B{ 		class D{ 	class C extends D{
 	  int z = 99; 		 int y;		 int x=10; B b = new B();
  	  B(){BODYB}	         D(){BODYD} 	 C(){super();BODYC}
        }			}		}
 	class T {
         public static void main(String args[]){
   	   C c = new C();
 	 }
        }
**)

(** Creates newly fresh allocated memory for some object of class 'c'.
  The list 'vl' of right values represents the actual parameters passed
  into the constructor 'm'. This method returns the reference of the
  newly allocated object, the store with the containing the new
  instance, the list of statements should be executed in order to
  finish the initialization.
**)
let rec new_inst s c vl =
  (** JSL-15.9.4 The new object contains new instances of all the fields
    declared in the specified class type and all its superclasses. As
    each new field instance is created, it is initialized to its default
    value.
  **)
  (*-* In the case of 'c' and its superclasses,	*-*)
  (*-* we create just one object 'pc'	 	*-*)
  let rec f c1 identt1 envmap1 = 
    let (identtcs,envmapcs)	=	
      (	List.fold_left
	  ( function (ids,fn) -> function clmb ->	
	      match clmb with
		| (jt,id)	->
		    ( IdentThis.add (Ident(id)) ids,
		      function x -> if x=Ident(id) then Some (init_val jt) else fn x
		    )
	  )
	  (identt1,envmap1)
	  (Prelude.fielddecl c1)
      )	in
      begin match Prelude.getSuper c1 with None -> (identtcs,envmapcs) | Some cls -> f cls identtcs envmapcs end in
  let pc = SetStore.cardinal s in
  let () = Printf.printf "%s " c in
  let () = Printf.printf "%i\n" pc in
  let idmap = f c (IdentThis.empty) (function x -> if x=ThisExp then Some (Oref(Nonnull(pc))) else None) in
  let envrm = Env(fst idmap, snd idmap) in
  let ss = SetStore.add (pc,c,envrm) s
  in (pc,ss,
      BlockIt(
	[ ExpStmt(MCall( StmtExp(ValS(Oref(Nonnull(pc)))),
			 CCId( CId( c,
				    ( List.fold_left 
					( function a -> function rv ->
					    match rv with
					      | Ival(_) -> a@[PrimType(IntType)]
					      | Bval(_) -> a@[PrimType(BoolType)]
					      | Oref(o) -> 
						  begin match getClass s o with
						    | Some ct -> a@[ClType(ct)]
						    | None -> raise (Object_Not_Found("<getClass>", 
										      (disp_obj ":" o Plenty)))
						  end
					)																						
					[]
					vl
				    )
				  )	
			     ),	
			 (rvalExprList vl)
		       )	
		 )	
	],
	envrm	
      )				
     )


(** establishes whether the class 'ct1' is a subclass of 'ct2'. **)
let rec isSubClass ct1 ct2 = 
  if ct1=ct2 then true
  else (** search in 'extends' and 'implements' classes of 'ct1' **)
    let f = function (* implementing class *)
	ctt ->	begin match Prelude.getImp ctt with
	  | None -> false
	  | Some cttil -> 
	      ( List.fold_left 
		  (function b -> function Implements(ctti) -> 
		     if ctti=ct2 then true 
		     else b || isSubClass ctti ct2 
		  ) 
		  false 
		  cttil
	      )								
	end
    and g = function (* extends class *)
	ctt ->	begin match Prelude.getSuper ctt with
	  | None -> false	
	  | Some ctte -> 
	      if ctte=ct2 then true
	      else isSubClass ctte ct2 
	end
    in (f ct1) || (g ct1)	

(** establishes whether the object 'o' has class 'ct' or not **)
let belongs s o = function
    ct ->
      match getClass s o with
	| None -> false
	| Some co -> co=ct || isSubClass co ct

;;

