(***********************************************************************)
(*                           Event Spaces                              *)
(*                                                                     *)
(*                     Néstor CATAÑO, Lemme project                    *)
(*                       INRIA Sophia Antipolis                        *)
(*                   2004 route des Lucioles-B.P. 93                   *)
(*                06902 Sophia Antipolis Cedex (France)                *)
(*                                                                     *)
(***********************************************************************)
open Javasyntax
open Display

module type OPERATIONAL = 
sig
  type config
  type mterm
  type objset

  type disp
  exception Not_Applicable of string * string
  exception Rewrite of string * string
  exception Event_Space_Error of string * string
  exception Initialization_Error 

  val isPopOutContext:	Store.t -> aterm -> bool
  val disp_objectset:	string -> objset -> displevel -> string
  val disp_mterm: 	string -> mterm  -> displevel -> string
  val disp_config: 	string -> config -> displevel -> string 
    
  val rew_store:	(mterm * config) -> (mterm * config)
  val rew_write:	(mterm * config) -> (mterm * config)
  val rew_read:		(mterm * config) -> (mterm * config)
  val rew_load:		(mterm * config) -> (mterm * config)
  val rew_one_step:	(mterm * config) -> (mterm * config)
  val rew_n_steps:	(mterm * config * out_channel) -> (mterm * config * out_channel)
  val run: compilunit -> compilunit list -> string -> displevel -> (mterm * config)
end
module Make: OPERATIONAL =
struct
  module Evtspace = Event_space.Make;;
  
  module Mterm = Set.Make(struct type t = thread * aterm * staterecord * Stack_of_env.t
				 let compare = function (th1,_,_,_) -> function (th2,_,_,_) ->
				   match th1,th2 with
				     |(Nullobj,Nullobj) -> 0
				     |(Nonnull(o1),Nonnull(o2)) -> compare o1 o2
				     |(Nonnull(o1),Nullobj) -> 1
				     |(Nullobj,Nonnull(o2)) -> -1
			  end		
			 );;
  type mterm = Mterm.t
		 
  module Objset = Set.Make(struct type t = obj let compare=compare end);;
  type objset = Objset.t
		  
  type config = (Mterm.elt * Objset.t * Evtspace.t * Store.t)
		  
  type disp = {mutable prf: string; mutable res: string}
		
  exception Not_Applicable of string * string
  exception Rewrite of string * string
  exception Event_Space_Error of string * string
  exception Initialization_Error
    
  (********************* Auxiliary functions **************************)
    
  (** establishes whether 'str' is a pop out context **)
  let isPopOutContext str = function
    | StatT(ReturnStmt(None)) -> true
	
    | ExprT(StmtExp(Ass(Field(StmtExp(ValS(Oref(Nonnull(_)))),_),e)))
    | ExprT(StmtExp(Ass(Field(e,_),_)))
    | ExprT(StmtExp(Ass(Var(_),e)))
    | ExprT(InstOf(e,_))
    | ExprT(UnOp(_,e))
    | ExprT(BinOp(StmtExp(ValS(_)),_,e))
    | ExprT(BinOp(e,_,_))
    | ExprT(Acc(Field(e,_)))
    | ExprT(StmtExp(MCall(e,_,_)))
    | StatT(VarDeclStmtE(_,_,e)) 
    | StatT(IfStmt(e,_))
    | StatT(ReturnStmt(Some e))
    | StatT(ThrowStmt(e))
    | StatT(SyncStmt(e,_)) ->
	begin match e with
	  | StmtExp(ReturnSE(_))
	  | StmtExp(ThrowSE(_)) -> true

	  | _ -> false
	end

    | StatT(BlockStmt(BlockIt(s::_,_)))
    | StatT(TryFinStmt(BlockIt([],_),_,BlockIt(s::_,_)))
    | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(_)))::_,_),[],BlockIt(s::_,_)))
    | StatT(TryFinStmt(BlockIt((ExpStmt(ReturnSE(_)))::_,_),_,BlockIt(s::_,_))) ->
	begin match s with
	  | ExpStmt(ReturnSE(_))
	  | ExpStmt(ThrowSE(_)) -> true

	  | _ -> false
	end
	
    | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::_,_),
		       (CC(ClType(ct),_,BlockIt(s::_,_)))::_,BlockIt([],_)))
    | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::_,_),
		       (CC(ClType(ct),_,BlockIt((ExpStmt(ThrowSE(_)))::_,_)))::_,BlockIt(s::_,_)))
    | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::_,_),
		       (CC(ClType(ct),_,BlockIt((ExpStmt(ReturnSE(_)))::_,_)))::_,BlockIt(s::_,_))) ->
	begin match s with
	  | ExpStmt(ReturnSE(_))
	  | ExpStmt(ThrowSE(_)) -> if Store.belongs str (Nonnull(p)) ct then true else false

	  | _ -> false
	end

    | StatT(s) ->
	begin match s with
	  | ExpStmt(ReturnSE(_))
	  | ExpStmt(ThrowSE(_)) -> true

	  | _ -> false
	end
	
    | _ -> false

  (** to display the objectset 'objs' **) 
  let disp_objectset pfix objs swt =
    let r = {prf=pfix; res="\n"} in
    let () = r.res <- r.res ^ r.prf
    and () = r.res <- r.res ^"|--"
    and () = r.prf <- r.prf ^"|  "
    and () = r.res <- r.res ^"OBJECT_SET" in
    let () =
      (	Objset.fold 
	  (function o -> function a ->
	     let () = r.res <- r.res ^(disp_obj r.prf o swt) in
	       ()
	  )
	  objs
	  ()
      ) in r.res 

  (** to display the mterm 'mt' **)
  let disp_mterm pfix mt swt =
    let r = {prf=pfix; res="\n"} in
    let () = r.res <- r.res ^ r.prf
    and () = r.res <- r.res ^"|--"
    and () = r.prf <- r.prf ^"|  "
    and () = r.res <- r.res ^"MTERM" in
    let () =
      (	Mterm.fold 
	  (function mm -> function a ->
	     match mm with
	       | (th,at,sr,soe) ->
		   let () = r.res <- r.res ^ r.prf
		   and () = r.res <- r.res ^"|--"
		   and () = r.prf <- r.prf ^"|  "
		   and () = r.res <- r.res ^(disp_thread r.prf th swt)
		   and () = r.res <- r.res ^(disp_aterm r.prf at swt)
		   and () = r.res <- r.res ^(disp_staterecord r.prf sr swt)
		   and () = r.res <- r.res ^(Stack_of_env.display soe r.prf swt) in
		     ()
	  )
	  mt	
	  ()
      ) in r.res

  (** to display the configuration 'cnf' **)
  let disp_config pfix cnf swt =
    let r = {prf=pfix; res="\n"} in
      match cnf with
	| ((th,at,sr,soe),objs,evs,str) ->
	    let () = r.res <- r.res ^ r.prf
	    and () = r.res <- r.res ^"|--"
	    and () = r.prf <- r.prf ^"|  "
	    and () = r.res <- r.res ^"CONFIGURATION"
	    and () = r.res <- r.res ^(disp_thread r.prf th Plenty)
	    and () = r.res <- r.res ^(disp_aterm r.prf at Plenty)
	    and () = r.res <- r.res ^(disp_staterecord r.prf sr Plenty)
	    and () = r.res <- r.res ^(Stack_of_env.display soe r.prf Plenty)
	    and () = r.res <- r.res ^(disp_objectset r.prf objs Plenty)
	    and () = r.res <- r.res ^(Evtspace.display evs r.prf swt)
	    and () = r.res <- r.res ^(Store.display str r.prf swt)
	    in r.res

  (*****************************************************************)


  (*-* pick up a configuration from a set of 'mterm *-*)
  let pickup_conf (mt,cnf) =
    match cnf with 
      |	((_,_,z,_),objset,evsp,str) -> 
	  if z=D then
	    begin try
	      let mt2 = (Mterm.filter (function (_,_,z,_) -> z<>D) mt) in
		(Mterm.choose mt2,objset,evsp,str)
	    with
		Not_found -> cnf
	    end	
	  else cnf


  (*****************************************************************)
	    
  (* store *)
  let rew_store = function
    | (mt,((th,e,R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *store*\n" in**)
	begin match Evtspace.getLastAssign evsp th with
	  | Some (Assign(_,l,v)) ->
	      begin match Evtspace.plusE evsp (Store(th,l,v)) with
		| Some evs -> (mt, ((th,e,R,stck),objs,evs,str))
		| None -> raise (Event_Space_Error("<rew_store,Event_space.plusE>",(disp_event ":" (Store(th,l,v)) Plenty )))
	      end	
	  | _	 -> raise (Event_Space_Error("<rew_store,Event_space.getLastAssign>",""))
	end
    | _ -> raise (Event_Space_Error("<rew_store,Event_space.getLastAssign>",""))

  (* write *)
  let rew_write = function
    | (mt,((th,e,R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *write*\n" in **)
	begin match Evtspace.getLastStore evsp th with
	  | Some (Store(_,l,v)) -> 
	      begin match Evtspace.plusE evsp (Write(th,l,v)) with
		| Some evs -> (mt, ((th,e,R,stck),objs,evs,(Store.update str l v)))
		| None	 -> raise (Event_Space_Error("<rew_write,Event_Space.plusE>",(disp_event ":" (Write(th,l,v)) Plenty)))
	      end	
	  | _	 -> raise (Event_Space_Error("<rew_write,Event_Space.getLastStore>",""))
	end
    | _	 -> raise (Event_Space_Error("<rew_write,Event_Space.getLastStore>",""))

  (* read *)
  let rew_read = function
    | (mt,((th,e,R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *read*\n" in**)
	let evsp3 =
	  (	List.fold_left
		  ( function evsp1 -> function 
		      | Write(t,l,v) ->
			  begin	match Evtspace.plusE evsp1 (Read(th,l,v)) with
			    |	Some evsp2	-> evsp2
			    | None				-> evsp1
			  end	
		      | _ -> evsp1	
		  )
		  evsp
		  (Evtspace.getLastWriteList evsp)
	  ) in (mt, ((th,e,R,stck),objs,evsp3,str))
    | _ -> raise (Event_Space_Error("<rew_read>",""))
					
  (* load *)
  let rew_load = function
    |	(mt,((th,e,R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *load*\n" in**)
	let evsp3 =
	    (	List.fold_left
		  ( function evsp1 -> function 
		      | Read(t,l,v) ->
			  begin	match Evtspace.plusE evsp1 (Load(th,l,v)) with
			    | Some evsp2	-> evsp2
			    | None		-> evsp1
			  end	
		      | _ -> evsp1	
		  )
		  evsp
		  (Evtspace.getLastReadList evsp)
	    ) in (mt, ((th,e,R,stck),objs,evsp3,str))
    | _ -> raise (Event_Space_Error("<rew_load>",""))
		
  (** one rewriting step. **)	
  let rec rew_one_step = function
      (* assign4 *)
    | (mt,((th,ExprT(StmtExp(Ass(Var(i),StmtExp(ValS(v))))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *assign4*\n" in**)
	begin	match (Stack_of_env.assign stck (Ident(i)) v) with
	  |	Some s -> (*-* The store should be also modified, otherwise the
			    the next time a method is reached, it will use
			    non updated values.
			    *-*)
		  begin match (Stack_of_env.lookTop stck ThisExp) with
		    | Some Oref(Nonnull(o)) -> 
			let elterm = (th,ExprT(StmtExp(ValS(v))),R,s) in
			  (Mterm.add elterm (Mterm.remove elterm mt),
			   (elterm,objs,evsp,(Store.update str (Lval(o,FId(i,ClType("")))) v)))
		    | _ -> raise (Rewrite("<rew_one_step,assign4,Stack_of_env.lookTop>",
					  (Stack_of_env.display stck "s:" Plenty)
					  ^(disp_identifier "i:" i Plenty)))
		  end
		  
	  | None	 -> raise (Rewrite("<rew_one_step,assign4,Stack_of_env.assign>",
					   (Stack_of_env.display stck "s:" Plenty)
					   ^(disp_identifier "i:" i Plenty)
					   ^(disp_rval "v:" v Plenty)))
	end
	
    (* assign2 *)
    | (mt1,((th,ExprT(StmtExp(Ass(Var(i),e1))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *assign2*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(e2) ->
		  let elterm = (th,ExprT(StmtExp(Ass(Var(i),e2))),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | _	-> raise (Not_Applicable("<rew_one_step,assign2>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* assign3 *) (* assign1 *) (* assign5 *)
    | (mt1,((th,ExprT(StmtExp(Ass(le,e))),R,stck1),objs1,evsp1,str1)) ->
	begin match lhsLval le with
	  | Some l -> (** assign5 **) (** assign3 **)
	      begin match e with
		| StmtExp(ValS(v)) -> (** assign5 **)
		    (**let () = Printf.printf "Rule *assign5*\n" in**)
		    begin match Evtspace.plusE evsp1 (Assign(th,l,v)) with
		      |	Some evs ->
			  let elterm = (th,ExprT(e),R,stck1) in
			    (Mterm.add elterm (Mterm.remove elterm mt1),
			     (elterm,objs1,evs,str1))
			    
		      | None -> raise (Rewrite("<rew_one_step,assign5,Event_space.plusE>",(disp_event ":" (Assign(th,l,v)) Plenty)))
		    end
		| _ -> 	(** assign3 **)
		    (**let () = Printf.printf "Rule *assign3*\n" in**)
		    let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e),R,stck1),objs1,evsp1,str1)) in
		      begin match ee with
			|	ExprT(e2) -> 
				  let elterm = (th,ExprT(StmtExp(Ass(le,e2))),z,stck2) in
				    (Mterm.add elterm (Mterm.remove elterm mt2),
		 		     (elterm,objs2,evsp2,str2))

			| _ -> raise (Not_Applicable("<rew_one_step,assign3>",(disp_aterm ":" ee Plenty)))
		      end
	      end	
	  | None -> (** assign1 **)
		(**let () = Printf.printf "Rule *assign1*\n" in**)
	      let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(Acc(le)),R,stck1),objs1,evsp1,str1)) in
		begin match ee with
		  | ExprT(Acc(e2)) ->
		      let elterm = (th,ExprT(StmtExp(Ass(e2,e))),z,stck2) in
			(Mterm.add elterm (Mterm.remove elterm mt2),
			 (elterm,objs2,evsp2,str2))

		  | _ -> raise (Not_Applicable("<rew_one_step,assign1>",(disp_aterm ":" ee Plenty)))
		end 
	end	

    (* var *)
    | (mt,((th,ExprT(Acc(Var(i))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *var*\n" in**)
	begin
	  match Stack_of_env.lookup stck (Ident(i)) with
	    |	Some v -> 
		  let elterm = (th,ExprT(StmtExp(ValS(v))),R,stck) in
		    (Mterm.add elterm (Mterm.remove elterm mt),
		     (elterm,objs,evsp,str))

	    | None	 -> raise (Rewrite(	"<rew_one_step,var,Stack_of_env.lookup>",
						(disp_identthis	"i" (Ident(i)) Plenty)
						^(Stack_of_env.display stck "s" Plenty)))
	end

    (* access2 *)
    |	(mt1,((th,ExprT(Acc(Field(StmtExp(ValS(Oref(Nullobj))),f))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *access2*\n" in**)
	  let (p,str2,bcknew) = Store.new_inst str1 "NullPointerException" [] in
	  let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck1) in
	    (Mterm.add elterm (Mterm.remove elterm mt1),
	     (elterm,objs1,evsp1,str2))

    (* access3 *) (* access1 *)
    |	(mt1,((th,ExprT(Acc(Field(e1,f)as le)),R,stck1),objs1,evsp1,str1)) ->
	  if isLval le then (** access3 **) 
	    (**let () = Printf.printf "Rule *access3*\n" in**)
	    begin	match lhsLval le with
	      | Some l ->
		  begin	match Store.getRval str1 l with
		    |	Some v ->
			  begin match Evtspace.plusE evsp1 (Use(th,l,v)) with
			    |	Some evs ->
				  let elterm = (th,ExprT(StmtExp(ValS(v))),R,stck1) in
				    (Mterm.add elterm (Mterm.remove elterm mt1),
				     (elterm,objs1,evs,str1))

			    | None -> raise (Rewrite("<rew_one_step,access3,Event_space.plusE>",(disp_event ":" (Use(th,l,v)) Plenty)))
			  end
		    | None -> raise (Rewrite("<rew_one_step,access3,Store.getRval>",
					     (*Store.display str1 "s:"*)
					     (disp_lval "l:" l Plenty)))
		  end
	      | None -> raise (Not_Applicable("<rew_one_step,access3>",(disp_lefthandside ":" le Plenty)))
	    end		
	  else (** access1 **)
	    (**let () = Printf.printf "Rule *access1*\n" in**)
	    let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	      begin match ee with
		| ExprT(e2) ->
		    let elterm = (th,ExprT(Acc(Field(e2,f))),z,stck2) in
		      (Mterm.add elterm (Mterm.remove elterm mt2),
		       (elterm,objs2,evsp2,str2))

		| _ -> raise (Not_Applicable("<rew_one_step,access1>",(disp_aterm ":" ee Plenty)))
	      end
	      
    (* this *)
    | (mt,((th,ExprT(This),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *this*\n" in**)
	begin	match Stack_of_env.lookup stck (ThisExp) with
	  |	Some v ->
		  let elterm = (th,ExprT(StmtExp(ValS(v))),R,stck) in
		    (Mterm.add elterm (Mterm.remove elterm mt),
		     (elterm,objs,evsp,str))

	  | None -> raise (Rewrite(	"<rew_one_step,this,Stack_of_env.lookup>",
					(Stack_of_env.display stck ":" Plenty)
					^"\n---------------------------"
					^(disp_identthis ":"	(ThisExp) Plenty)))
	end
	
    (* newc2 *) (* newc1 *)
    | (mt1,((th,ExprT(StmtExp(NewC(c,eE))),R,stck1),objs1,evsp1,str1)) ->
	begin match expRvalList eE with
	  | Some vl -> (** newc2 **)
	      (**let () = Printf.printf "Rule *newc2*\n" in**)
	      begin try
		let (p,str2,bcknew) = Store.new_inst str1 c vl in (** it modifies 'str1' **)
		let elterm = (th,StatSeqT([ExpStmt(ValS(Oref(Nonnull(p)))); BlockStmt(bcknew)]),R,stck1) in
		  (Mterm.add elterm (Mterm.remove elterm mt1),
		   (elterm,objs1,evsp1,str2))
	      with
		  Store.Class_Not_Found(_,_) -> raise (Rewrite("<rew_one_step,newc2,Store.new_inst>",
							       disp_aterm ":" (ExprT(StmtExp(NewC(c,eE)))) Plenty))
	      end
	      
	  | None -> (** newc1 **)	
	      (**let () = Printf.printf "Rule *newc1*\n" in**)
	      let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprSeqT(eE),R,stck1),objs1,evsp1,str1)) in
		begin match ee with
		  | ExprSeqT(eE2) ->
		      let elterm = (th,ExprT(StmtExp(NewC(c,eE2))),z,stck2) in
			(Mterm.add elterm (Mterm.remove elterm mt2),
			 (elterm,objs2,evsp2,str2))

		  | _ -> raise (Not_Applicable("<rew_one_step,newc1>",(disp_aterm ":" ee Plenty)))
		end
	end

    (* lit *)
    | (mt,((th,ExprT(Lit(k)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *lit*\n" in**)
	let elterm = (th,ExprT(StmtExp(ValS(Store.value k))),R,stck) in 
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
    (* instof2 *)	(* instof3 *) 
    | (mt,((th,ExprT(InstOf(StmtExp(ValS(Oref(Nonnull(p)))),c)),R,stck),objs,evsp,str)) ->
	if Store.belongs str (Nonnull(p)) c then (** instof2 **)
	  (**let () = Printf.printf "Rule *instof2*\n" in**)
	  let elterm = (th,ExprT(Lit(BoolLit(true))),R,stck) in
	    (Mterm.add elterm (Mterm.remove elterm mt),
	     (elterm,objs,evsp,str))
	else (** instof3 **)
	  (**let () = Printf.printf "Rule *instof3*\n" in**)
	  let elterm = (th,ExprT(Lit(BoolLit(false))),R,stck) in
	    (Mterm.add elterm (Mterm.remove elterm mt), 
	     (elterm,objs,evsp,str))
	    
    (* instof1 *)
    | (mt1,((th,ExprT(InstOf(e1,c)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *instof1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(e2) ->
		  let elterm = (th,ExprT(InstOf(e2,c)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),	
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,instof1>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* unop2 *)
    | (mt1,((th,ExprT(UnOp(f,StmtExp(ValS(v)))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *unop2*\n" in**)
	let elterm = (th,ExprT(StmtExp(ValS(f v))),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1),
	   (elterm,objs1,evsp1,str1))

    (* unop1 *)				
    | (mt1,((th,ExprT(UnOp(f,e1)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *unop1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(e2) ->
		  let elterm = (th,ExprT(UnOp(f,e2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,unop1>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* binop3 *) 
    | (mt1,((th,ExprT(BinOp(StmtExp(ValS(v1)),f,StmtExp(ValS(v2)))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *binop3*\n" in**)
	let elterm = (th,ExprT(StmtExp(ValS(f v1 v2))),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1), 
	   (elterm,objs1,evsp1,str1))
	  
    (* binop2 *)
    | (mt1,((th,ExprT(BinOp(StmtExp(ValS(v1)),f,e)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *binop2*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e),R,stck1),objs1,evsp1,str1)) in
 	  begin
	    match ee with
	      | ExprT(e2) ->
		  let elterm = (th,ExprT(BinOp(StmtExp(ValS(v1)),f,e2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,binop2>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* binop1 *)
    | (mt1,((th,ExprT(BinOp(e1,f,e)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *binop1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(e2) ->
		  let elterm = (th,ExprT(BinOp(e2,f,e)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,binop1>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* parseq2 *)
    | (mt1,((th,ExprSeqT((StmtExp(ValS(v)))::eE1),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *parseq2*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprSeqT(eE1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      | ExprSeqT(eE2) ->
		  let elterm = (th,ExprSeqT((StmtExp(ValS(v)))::eE2),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,parseq2>",(disp_aterm ":" ee Plenty)))
	  end

    (* parseq1 *)
    | (mt1,((th,ExprSeqT(e1::eE1),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *parseq1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      | ExprT(e2) ->
		  let elterm = (th,ExprSeqT(e2::eE1),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))

	      | _ -> raise (Not_Applicable("<rew_one_step,parseq1>",(disp_aterm ":" ee Plenty)))
	  end

    (* call4 *)
    | (mt,((th,ExprT(StmtExp(MCall(StmtExp(ValS(Oref(Nullobj))),m,_))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *call4*\n" in**)
	let (p,str2,bcknew) = Store.new_inst str "NullPointerException" [] in
	let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck) in 
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str2))
	  
    (* call3 *) (* call2 *)
    | (mt1,((th,ExprT(StmtExp(MCall(StmtExp(ValS(Oref(p))),m,eE))),R,stck1),objs1,evsp1,str1)) ->
	begin	match expRvalList eE with
	  | Some vl -> (** call3 **)
	      (**let () = Printf.printf "Rule *call3*\n" in**)
	      begin	match Store.frame str1 p m vl with
		| Some f -> 
		    let elterm = (th,ExprT(StmtExp(AFrame(fst f,snd f))),R,stck1) in
		      (Mterm.add elterm (Mterm.remove elterm mt1),
		       (elterm,objs1,evsp1,str1))
		| None -> let msg = 	(Store.display str1 "s:" Plenty)
					^	(disp_obj "o:" p Plenty)
					^	(disp_mcallid "m:" m Plenty)
					^	(disp_rval_list "rv:" vl Plenty)
		  in raise (Rewrite("<rew_one_step,call3,Store.frame>", msg))
	      end
	  | None -> (** call2 **)
	      (**let () = Printf.printf "Rule *call2*\n" in**)
	      let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprSeqT(eE),R,stck1),objs1,evsp1,str1)) in
		begin	match ee with
		  | ExprSeqT(eE2) ->
		      let elterm = (th,ExprT(StmtExp(MCall(StmtExp(ValS(Oref(p))),m,eE2))),z,stck2) in
			(Mterm.add elterm (Mterm.remove elterm mt2),
			 (elterm,objs2,evsp2,str2))
			
		  | _ -> raise (Not_Applicable("<rew_one_step,call2>",(disp_aterm ":" ee Plenty)))
		end
	end

    (* call1 *)
    | (mt1,((th,ExprT(StmtExp(MCall(e1,m,eE))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *call1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      | ExprT(e2) ->
		  let elterm = (th,ExprT(StmtExp(MCall(e2,m,eE))),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,call1>",(disp_aterm ":" ee Plenty)))
	  end


    (*****************************************************************)
    (********************** ACTIVATION FRAMES ************************)
    (*****************************************************************)
	  
    (* exit1 *)		
    | (mt,((th,StatT(ExpStmt(AFrame(m,BlockIt([],_)))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *exit1*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
    (* exit2 *)		
    | (mt,((th,StatT(ExpStmt(AFrame(m,BlockIt((ExpStmt(ReturnSE(None)))::sS,_)))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *exit2*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
    (* exit3 *)		
    | (mt,((th,ExprT(StmtExp(AFrame(m,BlockIt((ExpStmt(ReturnSE(Some v)))::sS,_)))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *exit3*\n" in**)
	let elterm = (th,ExprT(StmtExp(ValS(v))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
    (* frame *)
    | (mt1,((th,ExprT(StmtExp(AFrame(m,b1))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *frame*\n" in**)
	let (mt2,((_,bb,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
	  begin match bb with
	    | StatT(BlockStmt(b2)) ->
		let elterm = (th,ExprT(StmtExp(AFrame(m,b2))),z,stck2) in
		  (Mterm.add elterm (Mterm.remove elterm mt2),
		   (elterm,objs2,evsp2,str2))

	    | _ -> raise (Not_Applicable("<rew_one_step,frame>",(disp_aterm ":" bb Plenty)))
	  end


    (*****************************************************************)
    (******************* LOCAL VARIABLE DECLARATIONS *****************)
    (*****************************************************************)
	  
    (* decl *)
    | (mt,((th,StatT(VarDeclStmtN(t,i)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *decl*\n" in**)
	begin	match Stack_of_env.bind stck (Ident(i)) (Store.init_val t) with
	  |	Some s ->
		  let elterm = (th,StatT(Nop),R,s) in
		    (Mterm.add elterm (Mterm.remove elterm mt), 
		     (elterm,objs,evsp,str))
		    
	  | None -> raise (Rewrite(	"<rew_one_step,decl,Stack_of_env.bind>",""))
	end
	
    (* decle2 *)
    | (mt,((th,StatT(VarDeclStmtE(t,i,StmtExp(ValS(v)))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *decle2*\n" in**)
	begin	match Stack_of_env.bind stck (Ident(i)) v with
	  |	Some s ->
		  let elterm = (th,StatT(Nop),R,s) in
		    (Mterm.add elterm (Mterm.remove elterm mt), 
		     (elterm,objs,evsp,str))
		    
	  | None -> raise (Rewrite("<rew_one_step,decle2,Stack_of_env.bind>",""))
	end

    (* decle1 *)
    | (mt1,((th,StatT(VarDeclStmtE(t,i,e1)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *decle1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin	match ee with
	    | ExprT(e2) ->
		let elterm = (th,StatT(VarDeclStmtE(t,i,e2)),z,stck2) in 
		  (Mterm.add elterm (Mterm.remove elterm mt2), 
		   (elterm,objs2,evsp2,str2))
		  
	    (** <because of the definition of new_inst> **)
	    | StatSeqT(ExpStmt(ValS(v))::sS) ->
		(**let () = Printf.printf "Rule *because of the new definition of new_inst*\n" in**)
		let elterm = (th,StatSeqT((VarDeclStmtE(t,i,StmtExp(ValS(v))))::sS),z,stck2) in
		  (Mterm.add elterm (Mterm.remove elterm mt2), 
		   (elterm,objs2,evsp2,str2))
		  
	    | _ -> raise (Not_Applicable("<rew_one_step,decle1>",
					 (disp_expr ":" e1 Plenty)
					 ^"\n---------------------------"
					 ^(disp_aterm ":" ee Plenty)))
		
	  end

	  
    (*****************************************************************)
    (***** EXPRESSION STATEMENT, SKIP, CONDITIONAL AND ITERATION *****)
    (*****************************************************************)
	  
    (* skip *)
    | (mt1,((th,StatT(SemiCol),R,stck1),objs1,evsp1,str1)) -> (** skip **)
	(**let () = Printf.printf "Rule *skip*\n" in**)
	let elterm = (th,StatT(Nop),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1),
	   (elterm,objs1,evsp1,str1))
	  
    (* expstat2 *)	
    | (mt1,((th,StatT(ExpStmt(ValS(v))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *expstat2*\n" in**)
	let elterm = (th,StatT(Nop),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1), 
	   (elterm,objs1,evsp1,str1))
	  
    (* if2 *)
    | (mt,((th,StatT(IfStmt(StmtExp(ValS(Bval(true))),s)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *if2*\n" in**)
	let elterm = (th,StatT(s),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))
	  
    (* if3 *)
    | (mt,((th,StatT(IfStmt(StmtExp(ValS(Bval(false))),s)),R,stck),objs,evsp,str)) -> 
	(**let () = Printf.printf "Rule *if3*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))
	  
    (* if1 *)
    | (mt1,((th,StatT(IfStmt(e1,s)),R,stck1),objs1,evsp1,str1)) -> 
	(**let () = Printf.printf "Rule *if1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      | ExprT(e2) -> 
		  let elterm = (th,StatT(IfStmt(e2,s)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,if1>",(disp_aterm ":" ee Plenty)))
	  end
	  
    (* while *)
    | (mt,((th,StatT(WhileStmt(e,s)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *while*\n" in**)
	let elterm = (th,StatT(IfStmt(e,BlockStmt(BlockIt([s;WhileStmt(e,s)],Stack_of_env.createEnv())))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
	  
    (*****************************************************************)
    (******************* BLOCKS AND SYNCHRONIZATION ******************)
    (*****************************************************************)

    (* nop *)
    | (mt,((th,StatSeqT((Nop)::sS),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *nop*\n" in**)
	let elterm = (th,StatSeqT(sS),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))

    (* statseq *)
    | (mt1,((th,StatSeqT(s1::sS),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *statseq*\n" in**)
	let (mt2,((_,ss,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(s1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ss with
	      | StatT(s2) ->
		  let elterm = (th,StatSeqT(s2::sS),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))

	      (** <because of the definition of new_inst> **)
	      | StatSeqT(s2) ->
		  (**let () = Printf.printf "Rule *because of the definition of new_inst*\n" in**)
		  let elterm = (th,StatSeqT(s2@sS),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,statseq>",(disp_aterm ":" ss Plenty)))
	  end	

    (* block1 *)
    | (mt,((th,StatT(BlockStmt(BlockIt([],env))),R,stck),objs,evsp,str)) -> 
  	(**let () = Printf.printf "Rule *block1*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm,objs,evsp,str))
	  
    (* syn4 *) (* syn3 *)
    | (mt1,((th,StatT(SyncStmt(StmtExp(ValS(Oref(o))),b1)),R,stck1),objs1,evsp1,str1)) -> 
	let (mt2,((_,bb,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match bb with 
	      |	StatT(Nop) | 
		  ExprT(StmtExp(ValS(_))) |
		    StatT(ExpStmt(ValS(_))) |
		      ExprT(StmtExp(ThrowSE(Throw(_)))) |
			StatT(ExpStmt(ThrowSE(Throw(_)))) | 
			  ExprT(StmtExp(ReturnSE(_))) |
			    StatT(ExpStmt(ReturnSE(_))) -> (** syn4 **)
  		(**let () = Printf.printf "Rule *syn4*\n" in**)
		begin match Evtspace.plusE evsp2 (Unlock(th,o)) with
		      |	Some evs ->
			  let elterm = (th,bb,z,stck2) in
			    (Mterm.add elterm (Mterm.remove elterm mt2), 
			     (elterm,objs2,evs,str2))

		      |	None -> raise (Rewrite("<rew_one_step,syn4,Event_space.plusE>",(disp_event ":" (Unlock(th,o)) Plenty)))
		  end
		  
	      | StatT(BlockStmt(b2)) -> (** syn3 **)
		  (**let () = Printf.printf "Rule *syn3*\n" in**)
		  let elterm = (th,StatT(SyncStmt(StmtExp(ValS(Oref(o))),b2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,syn4,syn3>",(disp_aterm ":" bb Plenty)))
	  end
	  
    (* syn2 *) (* syn1 *)
    | (mt1,((th,StatT(SyncStmt(e1,b)),R,stck1),objs1,evsp1,str1)) -> 
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(StmtExp(ValS(Oref(o)))) -> (** syn2 **)
		  (**let () = Printf.printf "Rule *syn2*\n" in**)
		  begin match Evtspace.plusE evsp2 (Lock(th,o)) with
		      |	Some evs ->
			  let elterm = (th,StatT(SyncStmt(StmtExp(ValS(Oref(o))),b)),z,stck2) in
			    (Mterm.add elterm (Mterm.remove elterm mt2), 
			     (elterm,objs2,evs,str2))

		      |	None 		 -> raise (Rewrite("<rew_one_step,syn2,Event_space.plusE>",(disp_event ":" (Lock(th,o)) Plenty)))
		  end
	      | ExprT(e2) -> (** syn1 **)
		  (**let () = Printf.printf "Rule *syn1*\n" in**)
		  let elterm = (th,StatT(SyncStmt(e2,b)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,syn2,syn1>",(disp_aterm ":" ee Plenty)))
	  end



    (*****************************************************************)
    (********************** EXCEPTIONS AND RETURN ********************)
    (*****************************************************************)

    (* exit4 *)	
    | (mt,((th,StatT(BlockStmt(BlockIt((ExpStmt(ThrowSE(Throw(o))))::sS,_))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *exit4*\n" in**)
	let elterm = (th,StatT(ExpStmt(ThrowSE(Throw(o)))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))
	  
    (* throw2 *)
    | (mt1,((th,StatT(ThrowStmt((StmtExp(ValS(Oref(o)))))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *throw2*\n" in**)
	let elterm = (th,StatT(ExpStmt(ThrowSE(Throw(o)))),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1), 
	   (elterm,objs1,evsp1,str1))

    (* throw1 *)
    | (mt1,((th,StatT(ThrowStmt(e1)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *throw1*\n" in**)
	let (mt2,((_,ss,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	begin match ss with
	      | ExprT(e2) ->
		  let elterm = (th,StatT(ThrowStmt(e2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,throw1>",(disp_aterm ":" ss Plenty)))
	  end	

    (* block2 *)
    | (mt1,((th,StatT(BlockStmt(BlockIt(sS1,env1))),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *block2*\n" in**)
	let (mt2,((_,sSS,z,stck2),objs2,evsp2,str2)) = 
	  rew_one_step (mt1,((th,StatSeqT(sS1),R,Stack_of_env.push stck1 env1),objs1,evsp1,str1)) in
	  begin match sSS with
	    | StatSeqT(sS2)	->
		begin match Stack_of_env.pop stck2 with
		  | Some env2 ->
		      let elterm = (th,StatT(BlockStmt(BlockIt(sS2,env2))),z,stck2) in
			(Mterm.add elterm (Mterm.remove elterm mt2),
			 (elterm,objs2,evsp2,str2)) 
			
		  |	None  -> raise (Rewrite("<rew_one_step,block2,Stack_of_env.pop>",
						(Stack_of_env.display stck2 "s:" Plenty)
						^(disp_aterm "a1:" (StatSeqT(sS1)) Plenty)
						^(disp_aterm "a2:" sSS Plenty)))
		end
	    | _ -> raise (Not_Applicable("<rew_one_step,block2>",
					 (disp_aterm ":" (StatSeqT(sS1)) Plenty)
					 ^"\n---------------------------"
					 ^(disp_aterm ":" sSS Plenty)))
	  end		

    (* ret2 *)
    | (mt,((th,StatT(ReturnStmt(None)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *ret2*\n" inq**)
	let elterm = (th,StatT(ExpStmt(ReturnSE(None))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))

    (* ret2 *)
    | (mt,((th,StatT(ReturnStmt(Some StmtExp(ValS(v)))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *ret2*\n" in**)
	let elterm = (th,StatT(ExpStmt(ReturnSE(Some v))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))

    (* ret1 *)
    | (mt1,((th,StatT(ReturnStmt(Some e1)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *ret1*\n" in**)
	let (mt2,((_,ss,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(e1),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ss with
	      | ExprT(e2) ->
		  let elterm = (th,StatT(ReturnStmt(Some e2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
	      | _ -> raise (Not_Applicable("<rew_one_step,ret1>",(disp_aterm ":" ss Plenty)))
	  end	

    (* try1 *)
    | (mt,((th,StatT(TryStmt(BlockIt([],_),_,_)),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *try1*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))
	  
    (* try2 *) (** first part **)
    | (mt,((th,StatT(TryStmt(BlockIt((ExpStmt(ReturnSE(f)))::sS,_),_,_)),R,stck),objs,evsp,str)) -> 
	(**let () = Printf.printf "Rule *try2 first-part*\n" in**)
	let elterm = (th,StatT(ExpStmt(ReturnSE(f))),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,evsp,str))
	  
    (* try2 *) (** second part: finally clause **)
    (* fin3 *) 
    |(mt1,((th,StatT(TryFinStmt(BlockIt((ExpStmt(ReturnSE(f)))::sS,envr),hH,b)),R,stck1),objs1,evsp1,str1)) ->
       begin match b with
	 | BlockIt([],_) -> (** try2 **)
	     (**let () = Printf.printf "Rule *try2 second-part*\n" in**)
	     let elterm = (th,StatT(ExpStmt(ReturnSE(f))),R,stck1) in
	       (Mterm.add elterm (Mterm.remove elterm mt1), 
		(elterm,objs1,evsp1,str1))
	 | _ -> (* fin3 *) 
	     let (mt2,((_,bb,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b)),R,stck1),objs1,evsp1,str1)) in
	       begin
		 match bb with
		   | StatT(BlockStmt(b2)) -> (** fin3 **)
	     	       (**let () = Printf.printf "Rule *fin3*\n" in**)
		       let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ReturnSE(f)))::sS,envr),hH,b2)),z,stck2) in
			 (Mterm.add elterm (Mterm.remove elterm mt2), 
			  (elterm,objs2,evsp2,str2))
			 
		   | _ -> raise (Not_Applicable("<rew_one_step,fin3>",(disp_aterm ":" bb Plenty)))
	       end
       end	 

    (** b1 is throws **)
    (* try6  *)
    (* try5  *) (** first part **)
    (* try10 *) (** first part **)
    (* try9  *) (** first part **)
    | (mt1,((th,StatT(TryStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr),CC(ClType(ct),i,b1),hH)),R,stck1),objs1,evsp1,str1)) ->
	if Store.belongs str1 (Nonnull(p)) ct then (** try6 **) (** try5 **)
	  let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
	    begin match bc with
	      |	StatT(Nop) |
		  ExprT(StmtExp(ValS(_))) |
		    StatT(ExpStmt(ValS(_))) |
		      ExprT(StmtExp(ThrowSE(Throw(_)))) |
			StatT(ExpStmt(ThrowSE(Throw(_)))) | 
		 	  ExprT(StmtExp(ReturnSE(_))) |
		 	    StatT(ExpStmt(ReturnSE(_))) -> (** try6 **)
	     	  (**let () = Printf.printf "Rule *try6*\n" in**)
		  let elterm = (th,bc,z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      |	StatT(BlockStmt(b2)) ->	(** try5 **)
	     	  (**let () = Printf.printf "Rule *try5*\n" in**)
		  let elterm = (th,StatT(TryStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr),CC(ClType(ct),i,b2),hH)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,try6,try5>",(disp_aterm ":" bc Plenty)))
	    end
	else (** try10 **) (** try9 **)
	  if hH <> [] then (** try10 **)
	    (**let () = Printf.printf "Rule *try10*\n" in**)
	    let (mt2,((_,cc,z,stck2),objs2,evsp2,str2)) = 
	      rew_one_step (mt1,((th,StatT(TryStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr),List.hd hH, List.tl hH)),R,stck1),objs1,evsp1,str1)) in
		begin match cc with 
		  | StatT(Nop)
		  | ExprT(StmtExp(ValS(_)))
		  | StatT(ExpStmt(ValS(_)))
		  | ExprT(StmtExp(ThrowSE(Throw(_))))
		  | StatT(ExpStmt(ThrowSE(Throw(_))))
		  | ExprT(StmtExp(ReturnSE(_)))
		  |  StatT(ExpStmt(ReturnSE(_))) -> (** try10 **)
		       let elterm = (th,cc,z,stck2) in
			 (Mterm.add elterm (Mterm.remove elterm mt2), 
			  (elterm,objs2,evsp2,str2))
			 
		  | _ -> raise (Not_Applicable("<rew_one_step,try10>",(disp_aterm ":" cc Plenty)))
	      end
	  else (** hH = [] **) (** try9 **)
	    (**let () = Printf.printf "Rule *try9 with hH EMPTY*\n" in**)
	    let elterm = (th,StatT(ExpStmt(ThrowSE(Throw(Nonnull(p))))),R,stck1) in
	      (Mterm.add elterm (Mterm.remove elterm mt1), 
	       (elterm,objs1,evsp1,str1))
	      
    (** b1 is not throws, yet it can be rewritten as throws **)
    (* try4 *) (** first part **)
    (* try7 *)
    (* try8 *) (** first part **)
    (* try3 *) (** first part **)
    | (mt1,((th,StatT(TryStmt(b1,CC(ClType(ct),i,BlockIt(sP,envr1)),hH)),R,stck1),objs1,evsp1,str1)) ->
	let (mt2,((_,bb,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
	  begin match bb with
	      | StatT(BlockStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr2))) -> (** try4 **) (** try7 **) (** try8 **)
		  if Store.belongs str2 (Nonnull(p)) ct then (** try4 **)
		    (**let () = Printf.printf "Rule *try4*\n" in**)
		    let elterm = (th,StatT(TryStmt(	BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr2),
							CC(ClType(ct),i,BlockIt(sP,Stack_of_env.update envr1 (Ident(i)) (Oref(Nonnull(p))))),
							hH)),z,stck2) in
		      (Mterm.add elterm (Mterm.remove elterm mt2),
		       (elterm,objs2,evsp2,str2))
		  else (** try7 when hH=[] **) (** try8 when hH<>[]**)
		    (**let () = Printf.printf "Rule *try7 when hH EMPTY and try8 when different to EMPTY*\n" in**)
		    let elterm = (th,StatT(TryStmt(	BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr2),
							CC(ClType(ct),i,BlockIt(sP,envr1)),
							[])),z,stck2) in 
		      (Mterm.add elterm (Mterm.remove elterm mt2),
		       (elterm,objs2,evsp2,str2))

	      | StatT(BlockStmt(b2)) -> (* try3 *)
		  (**let () = Printf.printf "Rule *try3*\n" in**)
		  let elterm = (th,StatT(TryStmt(b2,CC(ClType(ct),i,BlockIt(sP,envr1)),hH)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,try4,try7,try8,try3>",(disp_aterm ":" bb Plenty)))
	  end
	  
	  
    (** fin1 **)	
    (** fin2 **)	
    | (mt1,((th,StatT(TryFinStmt(BlockIt([],envr),hH,b)),R,stck1),objs1,evsp1,str1)) ->
	let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b)),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match bc with
	      |	StatT(Nop)
	      | ExprT(StmtExp(ValS(_)))
	      | StatT(ExpStmt(ValS(_)))
	      | ExprT(StmtExp(ThrowSE(Throw(_))))
	      |	StatT(ExpStmt(ThrowSE(Throw(_))))
	      | ExprT(StmtExp(ReturnSE(_)))
	      | StatT(ExpStmt(ReturnSE(_))) -> (** fin2 **)
		  (**let () = Printf.printf "Rule *fin2*\n" in**)
		  let elterm = (th,bc,z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | StatT(BlockStmt(b2)) -> (** fin1 **) 
		  (**let () = Printf.printf "Rule *fin1*\n" in**)
		  let elterm = (th,StatT(TryFinStmt(BlockIt([],envr),hH,b2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2),
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,fin1,fin2>",(disp_aterm ":" bc Plenty)))
	  end
	  
    (** b1 is throws **)
    (* try5  *) (** second part:finally clause **)
    (* try10 *) (** second part:finally clause **)
    (* try8  *) (** second part:finally clause **)
    (* try9  *) (** second part:finally clause **)
    (* fin5  *)
    (* fin4  *)
    (* fin6  *)
    | (mt1,((th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),CC(ClType(ct),i,b1)::hH,b)),R,stck1),objs1,evsp1,str1)) ->
	if Store.belongs str1 (Nonnull(p)) ct then (** fin5 **) (** fin4 **) (** fin6 **) (** try5 **)
	  match b1 with
	    | BlockIt([],_) ->	(** fin4 **) (** fin5 **)
		let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b)),R,stck1),objs1,evsp1,str1)) in
		  begin
		    match bc with
		      | StatT(Nop)
		      | ExprT(StmtExp(ValS(_)))
		      | StatT(ExpStmt(ValS(_)))
		      | ExprT(StmtExp(ThrowSE(Throw(_))))
		      |	StatT(ExpStmt(ThrowSE(Throw(_))))
		      | ExprT(StmtExp(ReturnSE(_)))
		      | StatT(ExpStmt(ReturnSE(_))) -> (** fin5 **)
		          (**let () = Printf.printf "Rule *fin5*\n" in**)
			  let elterm = (th,bc,z,stck2) in
			    (Mterm.add elterm (Mterm.remove elterm mt2),
			     (elterm,objs2,evsp2,str2))
			    
		      | StatT(BlockStmt(b2)) -> (** fin4 **) 
		          (**let () = Printf.printf "Rule *fin4*\n" in**)
			  let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),CC(ClType(ct),i,b1)::hH,b2)),z,stck2) in
			    (Mterm.add elterm (Mterm.remove elterm mt2),
			     (elterm,objs2,evsp2,str2))
			    
		      | _ -> raise (Not_Applicable("<rew_one_step,fin4,fin5>",(disp_aterm ":" bc Plenty)))
		  end
	    | _ -> (** fin6 **) (** try5 **)
		  begin match b1 with
		    | BlockIt((ExpStmt(ThrowSE(_)))::sSP,envr2)
		    | BlockIt((ExpStmt(ReturnSE(_)))::sSP,envr2) -> (** fin6 **) 
			let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b)),R,stck1),objs1,evsp1,str1)) in
			  begin match bc with
			    | StatT(BlockStmt(b2)) -> (** fin6 **)
		        	(**let () = Printf.printf "Rule *fin6*\n" in**)
				let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),CC(ClType(ct),i,b1)::hH,b2)),z,stck2) in
				  (Mterm.add elterm (Mterm.remove elterm mt2),
				   (elterm,objs2,evsp2,str2))
				  
			    | _ -> raise (Not_Applicable("<rew_one_step,fin6>",(disp_aterm ":" bc Plenty)))
			  end
		    | _ -> (** try5 **)
			let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
			  begin match bc with
			    | StatT(BlockStmt(b2)) ->	(** try5 **)
		        	(**let () = Printf.printf "Rule *try5*\n" in**)
				let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),CC(ClType(ct),i,b2)::hH,b)),z,stck2) in
				  (Mterm.add elterm (Mterm.remove elterm mt2),
				   (elterm,objs2,evsp2,str2))
				  
			    | _ -> raise (Not_Applicable("<rew_one_step,try5>",(disp_aterm ":" bc Plenty)))
			  end
		end
	else (** try10 **) (** try8 **) (** try9 **)
	  if hH <> [] then (** try10 **) (** try8 **)
	    let (mt2,((_,cc,z,stck2),objs2,evsp2,str2)) = 
	      rew_one_step (mt1,((th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),hH,b)),R,stck1),objs1,evsp1,str1)) in
	      begin	
		match cc with 
		  | StatT(Nop)
		  | ExprT(StmtExp(ValS(_)))
		  | StatT(ExpStmt(ValS(_)))
		  | ExprT(StmtExp(ThrowSE(Throw(_))))
		  | StatT(ExpStmt(ThrowSE(Throw(_))))
		  | ExprT(StmtExp(ReturnSE(_)))
		  | StatT(ExpStmt(ReturnSE(_))) -> (** try10 **)
		      (**let () = Printf.printf "Rule *try10*\n" in**)
		      let elterm = (th,cc,z,stck2) in 
			(Mterm.add elterm (Mterm.remove elterm mt2), 
			 (elterm,objs2,evsp2,str2))
			
		  | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr2),hhH2,bb2)) -> (** try8 **)
		      (**let () = Printf.printf "Rule *try8*\n" in**)
		      let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr2),CC(ClType(ct),i,b1)::hhH2,bb2)),z,stck2) in
			(Mterm.add elterm (Mterm.remove elterm mt2),
			 (elterm,objs2,evsp2,str2))
			
		  | _ -> raise (Not_Applicable("<rew_one_step,try10,try8>",(disp_aterm ":" cc Plenty)))
	      end
	  else (** hH = [] **) (** try9 **) (** try8 **)
	    begin
	      match b with
		| BlockIt([],_) -> (** try9 **)
		    (**let () = Printf.printf "Rule *try9*\n" in**)
		    let elterm = (th,StatT(ExpStmt(ThrowSE(Throw(Nonnull(p))))),R,stck1) in 
		      (Mterm.add elterm (Mterm.remove elterm mt1),
		       (elterm,objs1,evsp1,str1))
		      
		| _ -> (** try8 **)
		    (**let () = Printf.printf "Rule *try8*\n" in**)
		    let (mt2,((_,tr,z,stck2),objs2,evsp2,str2)) =
		      rew_one_step (mt1,((th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr1),hH,b)),R,stck1),objs1,evsp1,str1)) in
		      begin match tr with
			| StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr2),hhH2,bb2)) ->
			    let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr2),CC(ClType(ct),i,b1)::hhH2,bb2)),z,stck2) in
			      (Mterm.add elterm (Mterm.remove elterm mt2),
			       (elterm,objs2,evsp2,str2))
			| _ -> raise (Not_Applicable("<rew_one_step,try8>",(disp_aterm ":" tr Plenty)))
		      end
	    end

    (** b1 is not throws, yet it can be rewritten as throws **)
    (* try4 *) (** second part: finally clause **)
    (* try8 *) (** second part: finally clause **)
    (* try3 *) (** second part: finally clause **)
    | (mt1,((th,StatT(TryFinStmt(b1,CC(ClType(ct),i,BlockIt(sP,envr1))::hH,b)),R,stck1),objs1,evsp1,str1)) ->
	let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
	  begin match bc with
	    | StatT(BlockStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr2))) -> (** try4 **) (** try8 **)
		if Store.belongs str2 (Nonnull(p)) ct then (** try4 **)
		  (**let () = Printf.printf "Rule *try4*\n" in**)
		  let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(Nonnull(p)))))::sS,envr2),
						    CC(ClType(ct),i,BlockIt(sP,Stack_of_env.update envr1 (Ident(i)) (Oref(Nonnull(p)))))::hH,
						    b)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		else (** try8 **)
		  (**let () = Printf.printf "Rule *try8*\n" in**)
		  let (mt3,((_,bb3,z3,stck3),objs3,evsp3,str3)) = rew_one_step (mt1,((th,StatT(TryFinStmt(b1,hH,b)),R,stck1),objs1,evsp1,str1)) in
	 	    begin match bb3 with
		      | StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr3),hhH,bb)) ->
			  let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(oo))))::ssS,envr3),CC(ClType(ct),i,BlockIt(sP,envr1))::hhH,bb)),z,stck3) in
			    (Mterm.add elterm (Mterm.remove elterm mt3),
			     (elterm,objs3,evsp3,str3))
			    
		      | _ -> raise (Not_Applicable("<rew_one_step,try8>",(disp_aterm ":" bb3 Plenty)))
	 	    end
	    | StatT(BlockStmt(b2)) -> (* try3, when there is some 'catch' clause *)
		(**let () = Printf.printf "Rule *try3*\n" in**)
		let elterm = (th,StatT(TryFinStmt(b2,CC(ClType(ct),i,BlockIt(sP,envr1))::hH,b)),z,stck2) in
		  (Mterm.add elterm (Mterm.remove elterm mt1),
		   (elterm,objs2,evsp2,str2))
		  
	    | _ -> raise (Not_Applicable("<rew_one_step,try4,try8,try3>",(disp_aterm ":" bc Plenty)))
	  end
	  
	  
    (* fin8 *) 
    (* try3 *) (** second part: finally clause, when there is no 'catch' clause **)
    (* fin7 *)
    | (mt1,((th,StatT(TryFinStmt(b1,[],b)),R,stck1),objs1,evsp1,str1)) ->
	begin
	  match b1 with 
	    | BlockIt((ExpStmt(ThrowSE(Throw(o))))::sS,envr1) ->
		let (mt2,((_,bb,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b)),R,stck1),objs1,evsp1,str1)) in
		  begin match bb with
		    |	StatT(BlockStmt(b2)) -> (** fin8 **)
			  (**let () = Printf.printf "Rule *fin8*\n" in**)
			  let elterm = (th,StatT(TryFinStmt(BlockIt((ExpStmt(ThrowSE(Throw(o))))::sS,envr1),[],b2)),z,stck2) in
			    (Mterm.add elterm (Mterm.remove elterm mt2),
			     (elterm,objs2,evsp2,str2))
			    
		    | _ -> (** **)
			(**let () = Printf.printf "Rule **\n" in**)
			let elterm = (th,bb,z,stck2) in
			  (Mterm.add elterm (Mterm.remove elterm mt2),
			   (elterm,objs2,evsp2,str2))
		  end
		  
	    | _ -> 
		let (mt2,((_,bc,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,StatT(BlockStmt(b1)),R,stck1),objs1,evsp1,str1)) in
		  begin match bc with
		    | StatT(BlockStmt(b2)) -> (** fin7: when b1 --> =throws **) (** try3: when b1 --> <>throws **)
			(**let () = Printf.printf "Rule *fin7: when be --> =throws; try3: when b1 --> <>throws*\n" in**)
			let elterm = (th,StatT(TryFinStmt(b2,[],b)),z,stck2) in 
			  (Mterm.add elterm (Mterm.remove elterm mt2), 
			   (elterm,objs2,evsp2,str2))

		    | _ -> raise (Not_Applicable("<rew_one_step,fin7,try3>",(disp_aterm ":" bc Plenty)))
		  end
	end

	
    (*****************************************************************)
    (**************************** THREADS ****************************)
    (*****************************************************************)
	
    (* start1 *) (* start2 *) (* start3 *)
    | (mt,((th1,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("start",Void,[])),[]))),R,stck1),
	   objs1,evsp1,str1)) ->
	
	(** if 'o' hasn't overriden the method 'start' **)
	if begin match (Store.getClass str1 o) with None -> false | Some c -> Store.methodbodyC c (MId("start",Void,[])) = None end then
	  (* start3 *)
	  (*-* if 'o' is already running an Exception must be raised *-*)
	  if (Mterm.exists (function (th,a,_,_) -> if th=o && a<>StatT(Nop) then true else false) mt) then
	    (**let () = Printf.printf "Rule *start3*\n" in**)
	    let (p2,s2,bcknew) = Store.new_inst str1 "IllegalThreadStateException" [] in
	    let elterm = (th1,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p2)))); BlockStmt(bcknew)]),R,stck1) in
	      (Mterm.add elterm (Mterm.remove elterm mt),
	       (elterm,objs1,evsp1,s2))
	  else
	    (*-* Standard case: if there is an user-defined 'run' method for 'o' and 'o' is not bound to die *-*)
	    (*-* JSL-20.20.14 Invoking the method 'start' of 'o' causes that 'o' begins execution.
	      The result is that two threads are running concurrently: the current thread ('th1')
	      ant the thread represented by this 'Thread' object ('o')
	      *-*)
	    (** start1 **)
	    if Store.frame str1 o (MCId(MId("run",Void,[]))) [] <> None && not(Objset.mem o objs1) then
	      (**let () = Printf.printf "Rule *start1*\n" in**)
		begin match Store.frame str1 o (MCId(MId("run",Void,[]))) [] with
		  | Some (mid,blck) -> 
		      let elterm = (o,StatT(ExpStmt(AFrame(mid,blck))),R,Stack_of_env.create()) in
		      let mt2 = Mterm.add elterm (Mterm.remove elterm mt) in
		      let elterm2 = (th1,StatT(Nop),R,stck1) in
			(Mterm.add elterm2 (Mterm.remove elterm2 mt2),
			 (elterm2, objs1,evsp1,str1))
			
		  | None  -> raise (Rewrite("<rew_one_step,start1,Store.frame>",
					    (disp_obj ":" o Plenty) ^"-+-"
					    ^(disp_methid ":" (MId("run",Void,[])) Plenty) ^"-+-"
					    ^(disp_rval_list ":" [] Plenty)))
	      end
	      (** start2 **)	 
	      (** if there is not an user-defined 'run' method for 'o' OR  **)
	      (** 'o' is bound to die.		 							                   **)
	    else
	      (**let () = Printf.printf "Rule *start2\n" in**)
	      let mt2 =	(** put all threads others than 'o' into 'Nop' **)
		( Mterm.fold 
		    (function elm -> function mmt -> 
		       match elm with
			 |	(th2,e2,R,stck2) -> if th2<>o & th2<>th1 then let elterm=(th2,StatT(Nop),R,stck2) in Mterm.add elterm (Mterm.remove elterm mmt) else mmt
			 | _ 				 			 -> mmt
		    ) 
		    mt 
		    mt
		) in
	      let elterm = (th1,StatT(Nop),R,stck1) in
		(Mterm.add elterm (Mterm.remove elterm mt2),
		 (elterm,Objset.remove th1 objs1,evsp1,str1))
		
	(** 'o' has been overriden **)
	else raise (Not_Applicable("<rew_one_step,start1,start2,start3>",
				   (disp_aterm ":" (StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("start",Void,[])),[])))) Plenty)))
	  
    (* stop1 *)
    | (mt,((th,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("stop",Void,[])),[]))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *stop1*\n" in**)
	let elterm = (th,StatT(Nop),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,Objset.add o objs, evsp,str)) 
	  
    (* isAlive1 *) (* isAlive2 *) (* isAlive3 *)
    | (mt1,((th1,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("isAlive",Void,[])),[]))),R,stck1),
	    objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *isAlive1-2-3*\n" in**)
	let res = (Mterm.for_all (function (th,_,z,_) -> if th=o then z<>D else true) mt1) in
	let elterm = (th1,ExprT(Lit(BoolLit(true))),R,stck1) in
	  (Mterm.add elterm (Mterm.remove elterm mt1),
	   (elterm,objs1,evsp1,str1))
	  
    (* join *)
    | (mt,((th,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("join",Void,[])),[]))),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *join*\n" in**)
	let elterm = 
	  (th,StatT(BlockStmt(BlockIt([WhileStmt(StmtExp(MCall(StmtExp(ValS(Oref(o))),MCId(MId("isAlive",Void,[])),[])),
						 SyncStmt( StmtExp(ValS(Oref(o))),
							   BlockIt([ExpStmt(MCall(	StmtExp(ValS(Oref(o))),
											MCId(MId("wait",Void,[])),
											[]
										 )
									   ) 
								   ],
								   Stack_of_env.createEnv()
								  )
							 )	
						)	
				      ],
				      Stack_of_env.update (Stack_of_env.createEnv()) (ThisExp) (Oref(o))
				     )
			     )	
		   ),R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt),
	   (elterm, objs,evsp,str))
	  
    (* wait1 *) (* wait2 *)
    | (mt,((th,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("wait",Void,[])),[]))),R,stck),objs,evsp,str)) ->
	let n = Evtspace.locks evsp th o in
	  if n>0 then (** wait2 **)
	    (**let () = Printf.printf "Rule *wait2*\n" in**)
	    let f evs times =
	      if times<= n then
		match Evtspace.plusE evs (Unlock(th,o)) with
		  | Some f -> f
		  | None -> evs (** ??? this option never should happen **)
	      else evs
	    in
	    let elterm = (th,StatT(Nop),W(o,n),stck) in
	      (Mterm.add elterm (Mterm.remove elterm mt), 
	       (elterm,objs,f evsp 1,str))
	  else (** wait1 **)
	    (**let () = Printf.printf "Rule *wait1*\n" in**)
	    let (p,s,bcknew) = Store.new_inst str "IllegalMonitorStateException" [] in
	    let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck) in
	      (Mterm.add elterm (Mterm.remove elterm mt),
	       (elterm,objs,evsp,s))

    (* notify1 *) (* notify2 *)
    | (mt,((th,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("notify",Void,[])),[]))),R,stck),objs,evsp,str)) ->
	let n = Evtspace.locks evsp th o in
	  if n>0 then (** notify2 **)
	    (**let () = Printf.printf "Rule *notify2*\n" in**)
	    let (cond2,mt2) =	
	      (	Mterm.fold 
		  (function elm -> function (cond,mmt) -> 
		     match elm with
		       |	(th2,e2,W(oo,nn),stck2) ->
				  if oo=o && not(cond) then
				    let elterm = (th2,e2,W(oo,nn),stck2) in (true,Mterm.add elterm (Mterm.remove elterm mt)) 
				  else (false,mmt)
		       | _ 				 			 -> (false,mmt)
		  ) 
		  mt 
		  (false,mt)
	      ) in
	    let elterm = (th,StatT(Nop),R,stck) in
	      (Mterm.add elterm (Mterm.remove elterm mt2),
	       (elterm,objs,evsp,str))
	  else (** notify1 **)
	    (**let () = Printf.printf "Rule *notify1*\n" in**)
	    let (p,s,bcknew) = Store.new_inst str "IllegalMonitorStateException" [] in
	    let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck) in 
	      (Mterm.add elterm (Mterm.remove elterm mt),
	       (elterm,objs,evsp,s))

    (* notifyAll1 *) (* notifyAll2 *)
    | (mt,((th,StatT(ExpStmt(MCall(StmtExp(ValS(Oref(o))),MCId(MId("notifyAll",Void,[])),[]))),R,stck),objs,evsp,str)) ->
	let n = Evtspace.locks evsp th o in
	  if n>0 then (** notifyAll2 **)
	    (**let () = Printf.printf "Rule *notifyAll2*\n" in**)
	    let mt2 =
	      (	Mterm.fold 
		  (function elm -> function mmt -> 
		     match elm with
		       |	(th2,e2,W(oo,nn),stck2) ->
				  if oo=o then
				    let elterm = (th2,e2,N(oo,nn),stck2) in Mterm.add elterm (Mterm.remove elterm mmt)
				  else mmt
		       | _ -> mmt
		  ) 
		  mt 
		  mt
	      ) in
	    let elterm = (th,StatT(Nop),R,stck) in
	      (Mterm.add elterm (Mterm.remove elterm mt2),
	       (elterm,objs,evsp,str))
	  else (** notifyiAll1 **)
	    (**let () = Printf.printf "Rule *notifyAll1*\n" in**)
	    let (p,s,bcknew) = Store.new_inst str "IllegalMonitorStateException" [] in
	    let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck) in
	      (Mterm.add elterm (Mterm.remove elterm mt),
	       (elterm,objs,evsp,s))
	      
    (* ready *) 
    | (mt,((th,e,N(o,n),stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *ready*\n" in**)
	let f evs times =
	  if times<= n then
	    match Evtspace.plusE evs (Unlock(th,o)) with
	      | Some f -> f
	      | None -> evs (** ??? this option never should happen **)
	  else evs in
	let elterm = (th,e,R,stck) in
	  (Mterm.add elterm (Mterm.remove elterm mt), 
	   (elterm,objs,f evsp 1,str))

    (* die *)
    | (mt,((th,(StatT(Nop) as dd),R,stck),objs,evsp,str))
	(* | (mt,((th,(StatT(ExpStmt(ValS(_))) as dd),R,stck),objs,evsp,str)) *) (** ??? this option is interfering with *expstat2* **)
    | (mt,((th,(StatT(ExpStmt(ThrowSE(Throw(_)))) as dd),R,stck),objs,evsp,str))
    | (mt,((th,(StatT(ExpStmt(ReturnSE(_))) as dd),R,stck),objs,evsp,str)) ->
	(**let () = Printf.printf "Rule *die*\n" in**)
	let evsp2 =
	  begin match  Evtspace.plusE evsp (Lock(th,th)) with
	    | Some f -> 
		begin match  Evtspace.plusE f (Unlock(th,th)) with
		  | Some g -> g
		  | None -> evsp (** ??? this option should not happen **)
		end			
	    | None -> evsp (** ??? this option should not happen **)
	  end in
	let elterm1 = (th,dd,D,stck)
	and mt2 = ( Mterm.fold 
		      (function elm -> function mmt -> 
			 match elm with
			   | (th2,e2,W(o,n),stck2) ->
			       if o=th2 then 
				 let elterm2 = (th2,e2,N(o,n),stck2) in Mterm.add elterm2 (Mterm.remove elterm2 mmt) 
			       else mmt
			   | _ -> mmt
		      )
		      mt
		      mt
		  ) in
	  (Mterm.add elterm1 (Mterm.remove elterm1 mt2),
	   (elterm1,objs,evsp2,str))

    (* expstat1 *)
    | (mt1,((th,StatT(ExpStmt(se1)),R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *expstat1*\n" in**)
	let (mt2,((_,ee,z,stck2),objs2,evsp2,str2)) = rew_one_step (mt1,((th,ExprT(StmtExp(se1)),R,stck1),objs1,evsp1,str1)) in
	  begin
	    match ee with
	      |	ExprT(StmtExp(se2)) -> 
		  let elterm = (th,StatT(ExpStmt(se2)),z,stck2) in
		    (Mterm.add elterm (Mterm.remove elterm mt2), 
		     (elterm,objs2,evsp2,str2))
		    
	      | _ -> raise (Not_Applicable("<rew_one_step,expstat1>",(disp_aterm ":" ee Plenty)))
	  end

    (* stop2 *)
    | (mt1,((th,trm,R,stck1),objs1,evsp1,str1)) ->
	(**let () = Printf.printf "Rule *stop2*\n" in**)
	if Objset.mem th objs1 && isRedex trm then
	  let (p,str2,bcknew) = Store.new_inst str1 "ThreadDeath" [] in
	  let elterm = (th,StatSeqT([ExpStmt(ThrowSE(Throw(Nonnull(p)))); BlockStmt(bcknew)]),R,stck1) in
	    (Mterm.add elterm (Mterm.remove elterm mt1),
	     (elterm,objs1,evsp1,str2)) 

	else raise (Not_Applicable("<rew_one_step,stop2>",(disp_aterm ":" trm Plenty)))

    (*-* choosing from mt an element not going to die *-*)
    |	(mt,((th,trm,D,stck),objset,evsp,str)) -> 
	(**let () = Printf.printf "Rule *choosing from mt an element not going to die*\n" in**)
	  begin try
	    let mt2 = (Mterm.filter (function (_,_,z,_) -> z<>D) mt) in
	      (mt,(Mterm.choose mt2,objset,evsp,str))
	  with
	      Not_found -> (mt,((th,trm,D,stck),objset,evsp,str))
	  end

    (* default *)
    | (_,((_,_,_,_),_,_,_)) -> 
	(**let () = Printf.printf "Rule *default*\n" in**)
	raise (Not_Applicable("<rew_one_step,default>",""))


  (** rewriting function **)	
  let rec rew_n_steps (mt,cnf,trace) =
    (*-* eliminate configurations having death threads *-*)
    (*-* choose an element not going to die from mt *-*)

    (*-*debugging instruction:*-*) 
    (*-*let () = Printf.fprintf trace "%s" (disp_config ":" cnf) in *-*)
    
    (*-* *-*)
    let cond1 = Mterm.is_empty mt = false
				      (*-* all threads of mt are dead *-*)
    and cond2 = ( Mterm.for_all 
		    (function (th,_,z,_) -> z=D)
		    mt
		)
		  (*-* evs is complete *-*)
    and cond3 = true (*-* ??? match cnf with (_,_,evsp,_) -> Evtspace.isComplete evsp *-*) in
      if cond1 && cond2 & cond3 then
	(mt,cnf,trace)
      else
	let (mt1,cnf1) =
	  begin	
	    try rew_store (mt,cnf)
	    with Event_Space_Error(_,_) -> (mt,cnf) 
	  end in
	let (mt2,cnf2) = 
	  begin	
	    try rew_write (mt1,cnf1)
	    with Event_Space_Error(_,_) -> (mt1,cnf1) 
	  end in
	let (mt3,cnf3) = 
	  begin 
	    try rew_read (mt2,cnf2)
	    with Event_Space_Error(_,_) -> (mt2,cnf2) 
	  end in
	let (mt4,cnf4) = 
	  begin 
	    try rew_load (mt3,cnf3)
	    with Event_Space_Error(_,_) -> (mt3,cnf3) 
	  end in
	let (mt5,cnf5) = rew_one_step (mt4,cnf4) in
	  rew_n_steps (mt5,cnf5,trace)
	    
  (** Running a Java program. The parameter 'u' is the main compilation
    unit of the program and the list of compilation units 'ul'
    represents the others java classes of the source code. The output 
    generated by the execution will be stored in the file named 'f' using a
    deployment type swt.
  **)
  let run u ul f swt =
    (** load the prelude **)
    let () = Prelude.load u in 
    let () = (List.iter	(function u -> Prelude.load u)  ul) in
      
    (** initial set of objects to die **)
    let empty_objs = Objset.empty in
    (** initial event space **)
    let empty_evsp = Evtspace.empty in
      
    (** The program starts with an instance of 'Thread' **)
    (** ??? here there is an error: bcknew should be executed
      before any other instruction **)
    let (th,str,bcknew) = Store.new_inst Store.create "Thread" [] in
      
      begin match u with
	| CompilC(_,_,Class(c,_,_,_)) ->
	    begin match Store.methodbody c (MId("main",Void,[ClType("String");])) with
	      | None -> raise Initialization_Error
	      | Some (BlockIt(sS,envrm),_) ->
		  let stck = Stack_of_env.create()
		  and mt = Mterm.empty in
		  let cnf = ((Nonnull(th),StatT(BlockStmt(BlockIt(sS,envrm))),R,stck),
				empty_objs,empty_evsp,str) in
		  let trace = open_out f in
		  let msg = 	"|=================================|\n"
				^"|========== EVENT SPACE ==========|\n"
				^"|=================================|\n" in
		  let () = Printf.fprintf trace "%s" msg in
		  let (mtr,cnfr,_) = rew_n_steps (mt,cnf,trace) in 
		  let () = Printf.fprintf trace "%s" (disp_config ":" cnfr swt) in
		  let _	= close_out trace in (mtr,cnfr)
	    end
	| _ -> raise Initialization_Error		
      end
end

;;
