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

exception Class_Not_Found 	of string * string
exception Not_Applicable	of string * string

val getCmpUn:	classtype -> compilunit option
val getImp: 	classtype -> (implements list) option
val getSuper:	classtype -> identifier option
val methoddecl: classtype -> methoddecl list
val constructordecl:	classtype -> constrdecl list
val fielddecl:		classtype -> fielddecl list
val load:	compilunit -> unit
;;
