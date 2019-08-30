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
(*          This file implements a stack of environments.              *)
(* This stack shriks and grows entering and exiting of blocks.         *)
(* This implementation is based on the Ocaml library Stack using a     *)
(* list mutable of data.																							 *)
(***********************************************************************) 
open Javasyntax
open Display

type t = { mutable c: env list}
type disp = {mutable prf: string; mutable res: string}

(** display the stack of environmnets 's' **) 
let display s pfix swt = 
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"STACK" in
  let rec f_display_list ll =
    begin match ll with
      | [] -> ()
      | e::es ->
	  let () = r.res <- r.res ^(disp_env r.prf e swt) in
	    f_display_list es
    end in
  let () = f_display_list s.c in r.res
		
(** creates an empty stack **) 
let create () = { c = [] }

(** creates an empty environment **)
let createEnv () = Env(IdentThis.empty,function _ -> None)

(** Updates the value of the identifier 'id' to 'r' in the environment
  'e'.  If 'id' does not exist in 'e' then it adds 'id' to 'e'.  It does
  not modify 'e'.  **)
let update e id r =
  match e with Env(idtset,f) -> Env(IdentThis.add id idtset,function x -> if x=id then Some r else f x)

(** Updates the identifiers of 'idl' to the values of 'rl'. **)
let updatelist e idl rl =
  ( List.fold_left2
      (function envrm -> function id -> function r -> update envrm id r)
      e
      idl
      rl
  )

	(*	List.fold_left2
		(function a -> function b -> function c -> update a b c)
		e
		idl
		rl
	*)

(** 'push' the environment 'e' on stack 's' **)
let push s e = s.c <- e::s.c; s

(** 'top' operation on stacks. This operation doesn't modify 's'.**)
let top s =
  match s.c with
    | hd::_ -> Some hd
    | [] -> None

(** 'pop' operation on stacks. It does modify 's' **)
let pop s = 
  match s.c with
    | hd::tl -> s.c <- tl; Some hd
    | [] -> None
	
(** Returns the value of the variable 'id' in the environment 'e' **)
let lookUp e id = match e with Env(_,f) -> f id

(** Returns the value of 'id' in the top of 's' **)
let lookTop s id =
  match (top s) with
    | Some e -> (lookUp e id)
    | None -> None
		
(** Binds the value 'r' to the identifier 'id' in the top level **)
(** environment of the stack 's'.                               **)
let bind s id r =
  match pop s with 
    |	Some e -> Some (push s (update e id r))
    | None	 -> None

(** search in the stack for the first environment of the stack 's' 
		where 'id' is declarated in the update it. 
		It modifies 's'.
**)
let assign s id r =
  let rec f ltemp = function
    | e::es ->
	begin match (lookUp e id) with
	  | Some _ ->	f (ltemp@[(update e id r)]) es
	  | None -> f (ltemp@[e])  es	
	end
    | [] -> let () = s.c <- ltemp in
	Some s
  in f [] s.c

(** seek the identifier 'id' in all environments of 's'. **)
let lookup s id =
  let rec f = function 
    | e::es ->
	begin match (lookUp e id) with
	  | Some r -> Some r 
	  | None  ->	f es 
	end 
    | [] -> None
  in f s.c 

       
;;
