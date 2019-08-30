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
(***********************************************************************) 
open Javasyntax
open Display

type t

val create: unit -> t
val createEnv: 	unit -> env
val update: env -> identthis -> rval -> env 
val updatelist: env -> identthis list -> rval list -> env
val push: t -> env -> t
val top: t -> (env option)
val pop: t -> (env option)
val lookUp: env -> identthis -> (rval option)
val lookTop: t -> identthis -> (rval option)
val bind: t -> identthis -> rval -> (t option)
val assign: t -> identthis -> rval -> (t option)
val lookup: t -> identthis -> (rval option)
val display: t -> string -> displevel -> string

;;
