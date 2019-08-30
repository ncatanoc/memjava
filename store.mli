(***********************************************************************)
(*                                                                    *)
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
(** A store 's' is composed by a list of tuplas <lval,rval>, where    **)
(** lval represents addresses of instance variables and rval its      **)
(** values.                                                           **)
(***********************************************************************)
open Javasyntax
open Display

type t

exception Class_Not_Found 	of string * string
exception Object_Not_Found	of string * string
exception Not_Applicable 	of string * string

val create:	t 
val value: 	literal -> rval
val init_val:	jtype -> rval
val getClass: 	t -> obj -> classtype option
val getEnvironment: t -> obj -> env option
val getRval:	t -> lval -> rval option
val update: 	t -> lval -> rval -> t
val methodbodyC:	classtype -> methid -> (block * identifier list) option
val methodbody:		classtype -> methid	-> (block * identifier list) option
val constructorbodyC:	classtype -> constrid	-> (block * identifier list) option
val constructorbody:	classtype -> constrid	-> (block * identifier list) option
val frame: 		t -> obj -> mcallid -> rval list -> (mcallid * block) option
val new_inst:		t -> classtype -> rval list -> (nonnullobj * t * block)
val isSubClass:	classtype -> classtype -> bool
val belongs:		t -> obj -> classtype -> bool
val display: 		t -> string -> displevel -> string
;;
