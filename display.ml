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
(* This file contains some utilities to display elements of the Java  **)
(* sintax.			                                  **)
(***********************************************************************)
open Javasyntax

type t = {mutable prf: string; mutable res: string}
	
type displevel =
 | Plenty
 | Light 

let disp_bool (pfix:string) (b:bool) : string =
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^(string_of_bool b)
  in r.res
		
let disp_int (pfix:string) (i:int) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^(string_of_int i)
	in r.res
 | Light ->
  let r = {prf=""; res=""} in
  let () = r.res <- r.res ^(string_of_int i)
  	in r.res
 end								

let disp_identifier (pfix:string) (id:identifier) (swt: displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^id 
  in r.res
 | Light ->
  let r = {prf=""; res=""} in
  let () = r.res <- r.res ^id 
  in r.res
 end

let disp_literal (pfix:string) (l:literal) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match l with
      | IntLit(i) 	-> 	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"IntLit"
				and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res
      | BoolLit(b)	-> let () = r.res <- r.res ^ r.prf
			   and () = r.res <- r.res ^"|--"
			   and () = r.prf <- r.prf ^"|  "
			   and () = r.res <- r.res ^"BoolLit"
			   and () = r.res <- r.res ^(disp_bool r.prf b)
	in r.res
	     
      | Null				-> let () = r.res <- r.res ^ r.prf
					   and () = r.res <- r.res ^"|--"
					   and () = r.prf <- r.prf ^"|  "
					   and () = r.res <- r.res ^"Null"
	in r.res

let disp_classtype (pfix:string) (id:string) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^id 
  in r.res
 | Light -> 
  let r = {prf=""; res=""} in
  let () = r.res <- r.res ^id 
  in r.res
 end

let disp_primitivetype (pfix:string) (pt:primitivetype) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match pt with
      | BoolType->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"BoolType"
	in r.res

      | IntType	->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"IntType" 
	in r.res
    end
 | Light ->
  let r = {prf=""; res=""} in
    begin match pt with
      | BoolType->	
			let () = r.res <- r.res ^"boolean"
			in r.res

      | IntType	->	
			let () = r.res <- r.res ^"int" 
			in r.res
     end
 end

let disp_jtype (pfix:string) (jt:jtype) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
   begin match jt with
      | PrimType(pt) -> let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"PrimType"
			and () = r.res <- r.res ^(disp_primitivetype r.prf pt swt)
			in r.res

      | ClType(ct)	-> let () = r.res <- r.res ^ r.prf
			   and () = r.res <- r.res ^"|--"
			   and () = r.prf <- r.prf ^"|  "
			   and () = r.res <- r.res ^"ClType"
			   and () = r.res <- r.res ^(disp_classtype r.prf ct swt)
	in r.res
   end
 | Light ->
  let r = {prf=""; res=""} in
    begin match jt with
      | PrimType(pt) -> 
		let () = r.res <- r.res ^(disp_primitivetype r.prf pt swt)
		in r.res

      | ClType(ct) -> 
		let () = r.res <- r.res ^(disp_classtype r.prf ct swt)
		in r.res
    end
 end

let disp_jtype_list (pfix:string) (jtl: (jtype list)) (swt:displevel) : string =
  let r = {prf=pfix; res=""} in
  let rec f_jtype_list ll =
    begin match ll with
      | [] -> () 
      | jt::jts ->
	  let () = r.res <- r.res ^(disp_jtype r.prf jt swt)
	  in f_jtype_list jts 
    end in
  let () = f_jtype_list jtl in r.res
		
let disp_returntype (pfix:string) (rt:returntype) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match rt with
      | RetType(jt)	->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"RetType"
				and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
	in r.res

      | Void	->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Void"
	in r.res

let disp_fieldidentifier (pfix:string) (fid:fieldidentifier) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match fid with 
      | FId(id,jt) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"FId"
			and () = r.res <- r.res ^(disp_identifier r.prf id swt)
			and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
	in r.res
    end
 | Light ->
  let r = {prf=""; res=""} in
    begin match fid with 
      | FId(id,jt) ->	
			let () = r.res <- r.res ^"FId("
			and () = r.res <- r.res ^(disp_identifier r.prf id swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
			and () = r.res <- r.res ^")"
	in r.res
    end

let disp_methid (pfix:string) (mid:methid) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match mid with 
      | MId(id,rt,jtl) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"MId"
				and () = r.res <- r.res ^(disp_identifier r.prf id swt)
				and () = r.res <- r.res ^(disp_returntype r.prf rt swt)
				and () = r.res <- r.res ^(disp_jtype_list r.prf jtl swt)
	in r.res

let disp_constrid (pfix:string) (cid:constrid) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match cid with 
      | CId(id,jtl) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"CId"
			and () = r.res <- r.res ^(disp_identifier r.prf id swt)
			and () = r.res <- r.res ^(disp_jtype_list r.prf jtl swt)
	in r.res

let disp_mcallid (pfix:string) (mcid:mcallid) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match mcid with
      | MCId(mid)	->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"MCId"
				and () = r.res <- r.res ^(disp_methid r.prf mid swt)
	in r.res
	     
      | CCId(cid) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"CCId"
			and () = r.res <- r.res ^(disp_constrid r.prf cid swt)
	in r.res

let disp_nonnullobj (pfix:string) (p:nonnullobj) (swt:displevel): string = 
	disp_int pfix p swt

let disp_obj (pfix:string) (o:obj) (swt:displevel) : string =
  match swt with
  | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match o with
      | Nonnull(p) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Nonnull"
			and () = r.res <- r.res ^(disp_nonnullobj r.prf p swt)
	in r.res
      | Nullobj	->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Nullobj"
	in r.res
    end
   | Light ->
  let r = {prf=""; res=""} in
    begin match o with
      | Nonnull(p) ->	
			let () = r.res <- r.res ^(disp_nonnullobj r.prf p swt)
				in r.res
      | Nullobj	->	
			let () = r.res <- r.res ^"null"
				in r.res
    end

let disp_lval (pfix:string) (l:lval) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match l with
      | Lval(p,fid) -> 	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Lval"
			and () = r.res <- r.res ^(disp_nonnullobj r.prf p swt)
			and () = r.res <- r.res ^(disp_fieldidentifier r.prf fid swt)
			in r.res
    end
 | Light ->
  let r = {prf=""; res=""} in
    begin match l with
      | Lval(p,fid) -> 	
			let () = r.res <- r.res ^"("
			and () = r.res <- r.res ^(disp_nonnullobj r.prf p swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_fieldidentifier r.prf fid swt)
			and () = r.res <- r.res ^")"
			in r.res
    end

let disp_rval (pfix:string) (rv:rval) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match rv with
      | Oref(o)	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"Oref"
		   and () = r.res <- r.res ^(disp_obj r.prf o swt)
	in r.res
	     
      | Ival(i)	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"Ival"
		   and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res
	     
      | Bval(b)	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"Bval"
		   and () = r.res <- r.res ^(disp_bool r.prf b)
			in r.res
    end
 | Light ->
  let r = {prf=""; res=""} in
    begin match rv with
      | Oref(o)	-> 
		let () = r.res <- r.res ^(disp_obj r.prf o swt)
		in r.res
	     
      | Ival(i)	-> 
		let () = r.res <- r.res ^(disp_int r.prf i swt)
		in r.res
	     
      | Bval(b)	-> 
		let () = r.res <- r.res ^(disp_bool r.prf b)
		in r.res
    end
 end
								
let disp_rval_option (pfix:string) (ro:(rval option)) (swt:displevel) : string =
 begin match swt with 
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match ro with
      | None ->	let () = r.res <- r.res ^ r.prf
		and () = r.res <- r.res ^"|--"
		and () = r.prf <- r.prf ^"|  "
		and () = r.res <- r.res ^"None"
	in r.res
      | Some v -> let () = r.res <- r.res ^ r.prf
		  and () = r.res <- r.res ^"|--"
		  and () = r.prf <- r.prf ^"|  "
		  and () = r.res <- r.res ^"Some"
		  and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res			
    end
  | Light ->
  let r = {prf=""; res=""} in
    begin match ro with
      | None ->	
		let () = r.res <- r.res ^"none"
		in r.res
      | Some v -> 
		let () = r.res <- r.res ^(disp_rval r.prf v swt)
		in r.res			
    end
 end
					
let disp_rval_list (pfix:string) (rvl:(rval list)) (swt:displevel) : string =
  let r = {prf=pfix; res=""} in
  let rec f_rval_list ll =
    begin match ll with
      | [] -> () 
      | rv::rvs ->
	  let () = r.res <- r.res ^(disp_rval r.prf rv swt) in
	    f_rval_list rvs 
    end in
  let () = f_rval_list rvl in r.res
				
and disp_lval_rval (pfix:string) (ide:(lval*rval)) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match ide with
	(l,v) ->
	  let () = r.res <- r.res ^ r.prf
	  and () = r.res <- r.res ^"|--"
	  and () = r.prf <- r.prf ^"|  "
	  and () = r.res <- r.res ^"LeftRight"
	  and () = r.res <- r.res ^(disp_lval r.prf l swt)
	  and () = r.res <- r.res ^(disp_rval r.prf v swt)
	  in r.res

let disp_throws (pfix:string) (t:throws) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match t with
	Throw(o) ->let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"Throw"
		   and () = r.res <- r.res ^(disp_obj r.prf o swt)
	in r.res
 
let disp_thread (pfix:string) (th:thread) (swt:displevel) : string =
 match swt with
  | Plenty -> 
  	let r = {prf=pfix; res="\n"} in
  	let () = r.res <- r.res ^ r.prf
  	and () = r.res <- r.res ^"|--"
  	and () = r.prf <- r.prf ^"|  "
  	and () = r.res <- r.res ^"Thread"
  	and () = r.res <- r.res ^(disp_obj r.prf th swt)
  		in r.res
  | Light  -> 
  	let r = {prf=""; res=""} in
  	let () = r.res <- r.res ^(disp_obj r.prf th swt)
  		in r.res

let disp_identthis (pfix:string) (idt:identthis) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match idt with
      | Ident(id)	-> let () = r.res <- r.res ^ r.prf
			   and () = r.res <- r.res ^"|--"
			   and () = r.prf <- r.prf ^"|  "
			   and () = r.res <- r.res ^"Ident"
			   and () = r.res <- r.res ^(disp_identifier r.prf id swt)
	in r.res

      | ThisExp	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"ThisExp" 
	in r.res
    end 
 | Light ->
  let r = {prf=""; res=""} in
    begin match idt with
      | Ident(id)-> 
		   let () = r.res <- r.res ^(disp_identifier r.prf id swt)
			in r.res

      | ThisExp	-> 
		   let () = r.res <- r.res ^"this" 
			in r.res
    end 
 end

let disp_identthis_envm (pfix:string) (idt:identthis) (envm:envmapping) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"."
  and () = r.res <- r.res ^(disp_identthis r.prf idt swt)
  and () = r.res <- r.res ^(disp_rval_option r.prf (envm(idt)) swt)
    in r.res
 | Light ->
  let r = {prf=""; res=""} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"("
  and () = r.res <- r.res ^(disp_identthis r.prf idt swt)
  and () = r.res <- r.res ^","
  and () = r.res <- r.res ^(disp_rval_option r.prf (envm(idt)) swt)
  and () = r.res <- r.res ^")"
    in r.res
 end

let disp_env (pfix:string) (ev:env) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match ev with
      | Env(idts,envm) -> 
	  let () = r.res <- r.res ^ r.prf
	  and () = r.res <- r.res ^"|--"
	  and () = r.prf <- r.prf ^"|  "
	  and () = r.res <- r.res ^"Env"
	  and () =	(IdentThis.fold
			  (function idt -> function a ->
			     r.res <- r.res ^(disp_identthis_envm r.prf idt envm swt)
			  )		
			  idts	
			  ()	
			)
	  in r.res 
    end
  | Light ->
   let r = {prf=""; res=""} in
    begin match ev with
      | Env(idts,envm) -> 
	let () = r.res <- r.res ^"("
	and () =(IdentThis.fold
		  (function idt -> function a ->
		     r.res <- r.res ^(disp_identthis_envm r.prf idt envm swt)
		  )		
		  idts	
		  ()	
		)
	and () = r.res <- r.res ^")"
	in r.res
    end
 end

let disp_store_element (pfix:string) (i:int) (id:identifier) (envr:env) (swt:displevel) : string =
 begin match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"."
  and () = r.res <- r.res ^(disp_int r.prf i swt)
  and () = r.res <- r.res ^(disp_identifier r.prf id swt)
  and () = r.res <- r.res ^(disp_env r.prf envr swt)
  in r.res
 | Light ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"("
  and () = r.res <- r.res ^(disp_int r.prf i swt)
  and () = r.res <- r.res ^","
  and () = r.res <- r.res ^(disp_identifier r.prf id swt)
  and () = r.res <- r.res ^","
  and () = r.res <- r.res ^(disp_env r.prf envr swt)
  and () = r.res <- r.res ^")"
  in r.res
 end								
								
let disp_event (pfix:string) (e:event) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match e with
      | Lock(th,o) -> let () = r.res <- r.res ^ r.prf
		      and () = r.res <- r.res ^"|--"
		      and () = r.prf <- r.prf ^"|  "
		      and () = r.res <- r.res ^"Lock"
		      and () = r.res <- r.res ^(disp_thread r.prf th swt)
		      and () = r.res <- r.res ^(disp_obj r.prf o swt)
	in r.res
	     
      | Unlock(th,o) ->let () = r.res <- r.res ^ r.prf
		       and () = r.res <- r.res ^"|--"
		       and () = r.prf <- r.prf ^"|  "
		       and () = r.res <- r.res ^"Unlock"
		       and () = r.res <- r.res ^(disp_thread r.prf th swt)
		       and () = r.res <- r.res ^(disp_obj r.prf o swt)
	in r.res
	     
      | Use(th,l,v) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Use"
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res
	     
      | Assign(th,l,v) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"Assign"
				and () = r.res <- r.res ^(disp_thread r.prf th swt)
				and () = r.res <- r.res ^(disp_lval r.prf l swt)
				and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res

      | Load(th,l,v) -> let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Load"
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res

      | Store(th,l,v) ->let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Store"
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res

      | Read(th,l,v) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Read"
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res

      | Write(th,l,v) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"Write"
				and () = r.res <- r.res ^(disp_thread r.prf th swt)
				and () = r.res <- r.res ^(disp_lval r.prf l swt)
				and () = r.res <- r.res ^(disp_rval r.prf v swt)
	in r.res
   end
 | Light ->
  let r = {prf=""; res=""} in
    begin match e with
      | Lock(th,o) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Lock("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_obj r.prf o swt)
			and () = r.res <- r.res ^")"
			in r.res
	     
      | Unlock(th,o) ->let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Unlock("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_obj r.prf o swt)
			and () = r.res <- r.res ^")"
			in r.res
	     
      | Use(th,l,v) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Use("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res
	     
      | Assign(th,l,v) ->let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Assign("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res

      | Load(th,l,v) -> let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Load("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res

      | Store(th,l,v) ->let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Store("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res

      | Read(th,l,v) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Read("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res

      | Write(th,l,v) ->let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"Write("
			and () = r.res <- r.res ^(disp_thread r.prf th swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_lval r.prf l swt)
			and () = r.res <- r.res ^","
			and () = r.res <- r.res ^(disp_rval r.prf v swt)
			and () = r.res <- r.res ^")"
			in r.res
  end

let disp_event_pair (pfix:string) (e1:event) (e2:event) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^"Event"
  and () = r.res <- r.res ^(disp_event r.prf e1 swt)
  and () = r.res <- r.res ^(disp_event r.prf e2 swt)
  in r.res

 | Light ->
  let r = {prf=pfix; res="\n"} in
  let () = r.res <- r.res ^ r.prf
  and () = r.res <- r.res ^"|--"
  and () = r.prf <- r.prf ^"|  "
  and () = r.res <- r.res ^(disp_event r.prf e1 swt)
  and () = r.res <- r.res ^"-->"
  and () = r.res <- r.res ^(disp_event r.prf e2 swt)
  in r.res


let rec disp_block (pfix:string) (b:block) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match b with
	BlockIt(stl,ev) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"BlockIt"
				and () = r.res <- r.res ^(disp_stat_list r.prf stl swt)
				and () = r.res <- r.res ^(disp_env r.prf ev swt)
	in r.res
	     
and disp_stat (pfix:string) (s:stat) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match s with
      | Nop -> let () = r.res <- r.res ^ r.prf
	       and () = r.res <- r.res ^"|--"
	       and () = r.prf <- r.prf ^"|  "
	       and () = r.res <- r.res ^"Nop"
	in r.res
	     
      | SemiCol	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"SemiCol"
	in r.res

      | BlockStmt(b) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"BlockStmt"
			and () = r.res <- r.res ^(disp_block r.prf b swt)
	in r.res
	     
      | ExpStmt(se) -> let () = r.res <- r.res ^ r.prf
		       and () = r.res <- r.res ^"|--"
		       and () = r.prf <- r.prf ^"|  "
		       and () = r.res <- r.res ^"ExpStmt"
		       and () = r.res <- r.res ^(disp_stmtexpr r.prf se swt)
	in r.res
	     
      | SyncStmt(e,b)	->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"SyncStmt"
				and () = r.res <- r.res ^(disp_expr r.prf e swt)
				and () = r.res <- r.res ^(disp_block r.prf b swt)
	in r.res
	     
      | IfStmt(e,s)	-> let () = r.res <- r.res ^ r.prf
			   and () = r.res <- r.res ^"|--"
			   and () = r.prf <- r.prf ^"|  "
			   and () = r.res <- r.res ^"IfStmt"
			   and () = r.res <- r.res ^(disp_expr r.prf e swt)
			   and () = r.res <- r.res ^(disp_stat r.prf s swt)
	in r.res
	     
      | WhileStmt(e,s) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"WhileStmt"
				and () = r.res <- r.res ^(disp_expr r.prf e swt)
				and () = r.res <- r.res ^(disp_stat r.prf s swt)
	in r.res

      | ThrowStmt(e) -> let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"ThrowStmt"
			and () = r.res <- r.res ^(disp_expr r.prf e swt)
	in r.res

      | TryStmt(b,c,cl) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"TryStmt"
				and () = r.res <- r.res ^(disp_block r.prf b swt)
				and () = r.res <- r.res ^(disp_catch r.prf c swt)
				and () = r.res <- r.res ^(disp_catch_list r.prf cl swt)
	in r.res

      | TryFinStmt(b1,cl,b2) -> let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"TryFinStmt"
				and () = r.res <- r.res ^(disp_block r.prf b1 swt)
				and () = r.res <- r.res ^(disp_catch_list r.prf cl swt)
				and () = r.res <- r.res ^(disp_block r.prf b2 swt)
	in r.res
	     
      | ReturnStmt(f) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"ReturnStmt"
				and () = r.res <- r.res ^(disp_expr_option r.prf f swt)
	in r.res
	     
      | VarDeclStmtN(jt,id)	->	let () = r.res <- r.res ^ r.prf
					and () = r.res <- r.res ^"|--"
					and () = r.prf <- r.prf ^"|  "
					and () = r.res <- r.res ^"VarDeclStmtN"
					and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
					and () = r.res <- r.res ^(disp_identifier r.prf id swt)
	in r.res

      | VarDeclStmtE(jt,id,e)	->	let () = r.res <- r.res ^ r.prf
					and () = r.res <- r.res ^"|--"
					and () = r.prf <- r.prf ^"|  "
					and () = r.res <- r.res ^"VarDeclStmtE"
					and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
					and () = r.res <- r.res ^(disp_identifier r.prf id swt)
					and () = r.res <- r.res ^(disp_expr r.prf e swt)
	in r.res

and disp_stat_list (pfix:string) (sl:(stat list)) (swt:displevel) : string =
  let r = {prf=pfix; res=""} in
  let rec f_stat_list ll =
    begin match ll with
      | [] -> () 
      | s::ss ->
	  let () = r.res <- r.res ^(disp_stat r.prf s swt) in
	    f_stat_list ss
    end in
  let () = f_stat_list sl	 in r.res
				      
and disp_stmtexpr (pfix:string) (se:stmtexpr) (swt:displevel) : string = 
  let r = {prf=pfix; res="\n"} in
    match se with
      | Ass(lhs,e) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Ass"
			and () = r.res <- r.res ^(disp_lefthandside r.prf lhs swt)
			and () = r.res <- r.res ^(disp_expr r.prf e swt)
	in r.res

      | NewC(ct,el) -> let () = r.res <- r.res ^ r.prf
		       and () = r.res <- r.res ^"|--"
		       and () = r.prf <- r.prf ^"|  "
		       and () = r.res <- r.res ^"NewC"
		       and () = r.res <- r.res ^(disp_classtype r.prf ct swt)
		       and () = r.res <- r.res ^(disp_expr_list r.prf el swt)
	in r.res
	     
      | MCall(e,mcid,el)	->	let () = r.res <- r.res ^ r.prf
					and () = r.res <- r.res ^"|--"
					and () = r.prf <- r.prf ^"|  "
					and () = r.res <- r.res ^"MCall"
					and () = r.res <- r.res ^(disp_expr r.prf e swt)
					and () = r.res <- r.res ^(disp_mcallid r.prf mcid swt)
					and () = r.res <- r.res ^(disp_expr_list r.prf el swt)
	in r.res

      | AFrame(mcid,b) ->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"AFrame"
				and () = r.res <- r.res ^(disp_mcallid r.prf mcid swt)
				and () = r.res <- r.res ^(disp_block r.prf b swt)
	in r.res

      | ValS(rv) -> 	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"ValS"
			and () = r.res <- r.res ^(disp_rval r.prf rv swt)
	in r.res

      | ThrowSE(thw) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"ThrowSE"
			and () = r.res <- r.res ^(disp_throws r.prf thw swt)
	in r.res
	     
      | ReturnSE(f) -> 	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"ReturnSE"
			and () = r.res <- r.res ^(disp_rval_option r.prf f swt)
	in r.res

and disp_expr (pfix:string) (e:expr) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match e with
      | Lit(lt)	-> 	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Lit"
			and () = r.res <- r.res ^(disp_literal r.prf lt swt)
	in r.res
	     
      | Acc(lhs) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Acc"
			and () = r.res <- r.res ^(disp_lefthandside r.prf lhs swt)
	in r.res

      | This -> let () = r.res <- r.res ^ r.prf
		and () = r.res <- r.res ^"|--"
		and () = r.prf <- r.prf ^"|  "
		and () = r.res <- r.res ^"This"
	in r.res
	     (*
	       | Super ->  let () = r.res <- r.res ^ r.prf
	       and () = r.res <- r.res ^"|--"
	       and () = r.prf <- r.prf ^"|  "
	       and () = r.res <- r.res ^"Super"
	       in r.res
	     *)
      | InstOf(e,c)	->	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"InstOf"
				and () = r.res <- r.res ^(disp_expr r.prf e swt)
				and () = r.res <- r.res ^(disp_classtype r.prf c swt)
	in r.res
	     
      | UnOp(uo,e) ->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"UnOp"
			and () = r.res <- r.res ^"UnaryOp"
			and () = r.res <- r.res ^(disp_expr r.prf e swt)
	in r.res

      | BinOp(e1,bo,e2)	-> 	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"BinOp"
				and () = r.res <- r.res ^(disp_expr r.prf e1 swt)
				and () = r.res <- r.res ^"BinaryOp"
				and () = r.res <- r.res ^(disp_expr r.prf e2 swt)
	in r.res

      | StmtExp(se)	-> 	let () = r.res <- r.res ^ r.prf
				and () = r.res <- r.res ^"|--"
				and () = r.prf <- r.prf ^"|  "
				and () = r.res <- r.res ^"StmtExp"
				and () = r.res <- r.res ^(disp_stmtexpr r.prf se swt)
	in r.res
	     
and disp_expr_option (pfix:string) (eo:(expr option)) (swt:displevel) : string =
  let r = {prf=pfix; res=""} in
    match eo with
      | None ->	let () = r.res <- r.res ^ r.prf
		and () = r.res <- r.res ^"|--"
		and () = r.prf <- r.prf ^"|  "
		and () = r.res <- r.res ^"None"
	in r.res
      | Some e -> let () = r.res <- r.res ^ r.prf
		  and () = r.res <- r.res ^"|--"
		  and () = r.prf <- r.prf ^"|  "
		  and () = r.res <- r.res ^"None"
		  and () = r.res <- r.res ^(disp_expr r.prf e swt)
	in r.res			
	     
and disp_expr_list (pfix:string) (el:(expr list)) (swt:displevel) : string = 
  let r = {prf=pfix; res=""} in
  let rec f_expr_list ll =
    begin match ll with
      | [] -> () 
      | e::es ->
	  let () = r.res <- r.res ^(disp_expr r.prf e swt) in
	    f_expr_list es
    end in
  let () = f_expr_list el in r.res
			       
and disp_identifier_expr (pfix:string) (ide:(identifier*expr)) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match ide with
	(id,e) ->
	  let () = r.res <- r.res ^ r.prf
	  and () = r.res <- r.res ^"|--"
	  and () = r.prf <- r.prf ^"|  "
	  and () = r.res <- r.res ^"Tupla"
	  and () = r.res <- r.res ^(disp_identifier r.prf id swt)
	  and () = r.res <- r.res ^(disp_expr r.prf e swt)
	  in r.res
    end
 | Light ->
  let r = {prf=""; res=""} in
    begin match ide with
	(id,e) ->
	  let () = r.prf <- r.prf ^"("
	  and () = r.res <- r.res ^(disp_identifier r.prf id swt)
	  and () = r.prf <- r.prf ^","
	  and () = r.res <- r.res ^(disp_expr r.prf e swt)
	  and () = r.prf <- r.prf ^")"
	  in r.res
    end
and disp_identifier_expr_list (pfix:string) (idel:((identifier*expr)list)) (swt:displevel) : string = 
  let r = {prf=pfix; res=""} in
  let rec f_identifier_expr_list ll =
    begin match ll with
      | [] -> () 
      | ide::ides ->
	  let () = r.res <- r.res ^(disp_identifier_expr r.prf ide swt) in
	    f_identifier_expr_list ides
    end in
  let () = f_identifier_expr_list idel in r.res
					    
and disp_catch (pfix:string) (cc:catch) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match cc with
      | CC(jt,id,b) -> let () = r.res <- r.res ^ r.prf
		       and () = r.res <- r.res ^"|--"
		       and () = r.prf <- r.prf ^"|  "
		       and () = r.res <- r.res ^"CC"
		       and () = r.res <- r.res ^(disp_jtype r.prf jt swt)
		       and () = r.res <- r.res ^(disp_identifier r.prf id swt)
		       and () = r.res <- r.res ^(disp_block r.prf b swt)
	in r.res
	     
and disp_catch_list (pfix:string) (ctl:(catch list)) (swt:displevel) : string = 
  let r = {prf=pfix; res=""} in
  let rec f_catch_list ll =
    begin match ll with
      | [] -> () 
      | ct::cts ->
	  let () = r.res <- r.res ^(disp_catch r.prf ct swt) in
	    f_catch_list cts
    end in
  let () = f_catch_list ctl in r.res
				 
and disp_lefthandside (pfix:string) (lhs:lefthandside) (swt:displevel) : string =
  let r = {prf=pfix; res="\n"} in
    match lhs with 
      |	Var(id)	-> let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"Var"
		   and () = r.res <- r.res ^(disp_identifier r.prf id swt)
	in r.res
      | Field(e,fid) -> let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"Field"
			and () = r.res <- r.res ^(disp_expr r.prf e swt)
			and () = r.res <- r.res ^(disp_fieldidentifier r.prf fid swt)
	in r.res

let disp_aterm (pfix:string) (at:aterm) (swt:displevel) : string =
	let r = {prf=pfix; res="\n"} in
	match at with 
	| ExprT(e) -> let () = r.res <- r.res ^ r.prf
						and () = r.res <- r.res ^"|--"
						and () = r.prf <- r.prf ^"|  "
						and () = r.res <- r.res ^"ExprT"
						and () = r.res <- r.res ^(disp_expr r.prf e swt)
							in r.res

	| ExprSeqT(el) -> let () = r.res <- r.res ^ r.prf
						and () = r.res <- r.res ^"|--"
						and () = r.prf <- r.prf ^"|  "
						and () = r.res <- r.res ^"ExprSeqT"
						and () = r.res <- r.res ^(disp_expr_list r.prf el swt)
							in r.res

	| StatT(s) ->  let () = r.res <- r.res ^ r.prf
						and () = r.res <- r.res ^"|--"
						and () = r.prf <- r.prf ^"|  "
						and () = r.res <- r.res ^"StatT"
						and () = r.res <- r.res ^(disp_stat r.prf s swt)
							in r.res

	| StatSeqT(sl) ->	 let () = r.res <- r.res ^ r.prf
						and () = r.res <- r.res ^"|--"
						and () = r.prf <- r.prf ^"|  "
						and () = r.res <- r.res ^"StatSeqT"
						and () = r.res <- r.res ^(disp_stat_list r.prf sl swt)
							in r.res

let disp_staterecord (pfix:string) (sr:staterecord) (swt:displevel) : string =
 match swt with
 | Plenty ->
  let r = {prf=pfix; res="\n"} in
    begin match sr with 
      |	R ->let () = r.res <- r.res ^ r.prf
	    and () = r.res <- r.res ^"|--"
	    and () = r.prf <- r.prf ^"|  "
	    and () = r.res <- r.res ^"R"
	in r.res
      | W(o,i) ->  let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"W"
		   and () = r.res <- r.res ^(disp_obj r.prf o swt)
		   and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res

      | N(o,i) ->   let () = r.res <- r.res ^ r.prf
		    and () = r.res <- r.res ^"|--"
		    and () = r.prf <- r.prf ^"|  "
		    and () = r.res <- r.res ^"N"
		    and () = r.res <- r.res ^(disp_obj r.prf o swt)
		    and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res
	     
      | D	->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"D"
	in r.res
   end
 | Light ->
  let r = {prf=""; res=""} in
    begin match sr with 
      |	R ->let () = r.res <- r.res ^ r.prf
	    and () = r.res <- r.res ^"|--"
	    and () = r.prf <- r.prf ^"|  "
	    and () = r.res <- r.res ^"R"
	in r.res
      | W(o,i) ->  let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^"W"
		   and () = r.res <- r.res ^(disp_obj r.prf o swt)
		   and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res

      | N(o,i) ->   let () = r.res <- r.res ^ r.prf
		    and () = r.res <- r.res ^"|--"
		    and () = r.prf <- r.prf ^"|  "
		    and () = r.res <- r.res ^"N"
		    and () = r.res <- r.res ^(disp_obj r.prf o swt)
		    and () = r.res <- r.res ^(disp_int r.prf i swt)
	in r.res
	     
      | D	->	let () = r.res <- r.res ^ r.prf
			and () = r.res <- r.res ^"|--"
			and () = r.prf <- r.prf ^"|  "
			and () = r.res <- r.res ^"D"
	in r.res
    end
;;
