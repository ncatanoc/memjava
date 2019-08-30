(***********************************************************************)  
(*                           Event Spaces                              *)
(*                                                                     *)
(*                     Néstor CATAÑO, Lemme project                    *)
(*                       INRIA Sophia Antipolis                        *)
(*                   2004 route des Lucioles-B.P. 93                   *)
(*                06902 Sophia Antipolis Cedex (France)                *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(*                This file implements an Event Space.                 *)
(***********************************************************************)

(***********************************************************************)
(* An Event Space is implemented as a set of (event * event) elements. *)
(***********************************************************************)
open Javasyntax
open Display

module type EVENTSPACE =
sig
  type t
    
  val empty: t
  val isMember: t -> (event * event) -> bool
  val add: t -> (event * event) -> t
  val elements: t -> (event * event) list
  val isSubset: t -> t -> bool
  val isEqual: t -> t -> bool
  val union: t -> t -> t
  val intersection: t -> t -> t
  val fold: t -> ((event * event) -> 'a -> 'a) -> 'a -> 'a
  val forAll: t -> ((event * event) -> bool) -> bool
  val exists: t -> ((event * event) -> bool) -> bool
  val filter: t -> ((event * event) -> bool) -> t

  type f

  val empty_f: f
  val isMember_f: f -> event -> bool
  val add_f: f -> event -> f
  val elements_f: f -> event list
  val isSubset_f: f -> f -> bool
  val isEqual_f: f -> f -> bool
  val union_f: f -> f -> f
  val intersection_f: f -> f -> f
  val fold_f: f -> (event -> 'a -> 'a) -> 'a -> 'a
  val forAll_f: f -> (event -> bool) -> bool
  val exists_f: f -> (event -> bool) -> bool
  val filter_f: f -> (event -> bool) -> f

  val carrier: t -> f
  val isRelated: t -> event -> event -> bool
  val isReflexive: f -> bool
  val isTransitive: f -> bool
  val isAntisimetric: f -> bool
  val isTotal: t -> f -> bool
  val display: t -> string -> displevel -> string

  val getLastAssign: t -> thread -> (event option)
  val getLastStore: 	t -> thread -> (event option)
  val getLastWriteList: 	t -> (event list) 
  val getLastReadList: 	t -> (event list)
    
  val locks: t -> thread -> obj -> int
  val isComplete: t -> bool
    
  val alpha : thread -> event -> bool
  val betal : lval -> event -> bool
  val beta : obj -> event -> bool
    
  val read_of: t -> event -> event option
  val store_of: t -> event -> event option

  val rule1: t -> f -> bool
  val rule2l: t -> f -> bool
  val rule2o: t -> f -> bool
  val rule3: t -> f -> bool
  val rule4: t -> f -> bool
  val rule5: t -> f -> bool
  val rule6: t -> f -> bool
  val rule7: t -> f -> bool
  val rule8: t -> f -> bool
  val rule9: t -> f -> bool
  val rule10: t -> f -> bool
  val rule11: t -> f -> bool
  val rule12: t -> f -> bool
  val rule13: t -> f -> bool
  val rule14: t -> f -> bool
  val rule15: t -> f -> bool
  val rule16: t -> f -> bool
  val rule17: t -> f -> bool 

  val isEventSpace: t -> f -> bool
  (*-*val extends: t -> t -> bool*-*)
  val getRsCandidates: t -> f -> event -> t list
  val adjointSet: t -> f -> event -> t list
  val plusE: t -> event -> (t option)

  type disp

end

module Make: EVENTSPACE =
struct
  module Elt = Set.Make(struct type t = (event * event) let compare = compare end);;
  type t = Elt.t (* (event * event) Set.t *)

  module Felt = Set.Make(struct type t = event let compare = compare end);;
  type f = Felt.t (* event Set.t *)
      
  module Lelt = Set.Make(struct type t = lval let compare = compare end);;
  module Relt = Set.Make(struct type t = rval let compare = compare end);;
  module Oelt = Set.Make(struct type t = obj let compare = compare end);;
  module Telt = Set.Make(struct type t = thread let compare = compare end);;

  type disp = {mutable prf: string; mutable res: string}
      
  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with the type t.                                                  **)
  (***********************************************************************)   

  (** Constructs a empty set  **)
  let empty = Elt.empty

  (** is 'x' member of 'xs' ? **)
  let isMember xs x = Elt.mem x xs

  (** adds 'x' to 'xs' provided that x isn't already **)
  let add xs x = Elt.add x xs
		   
  (** converts a set into a list **)
  let elements xs = Elt.elements xs

  (** is xset subset of yset ? **)
  let isSubset xs ys = Elt.subset xs ys

  (** are the sets 'xs' and 'ys' equals ? **)
  let isEqual xs ys = Elt.equal xs ys

  (** union of sets **)
  let union xs ys = Elt.union xs ys
		      
  (** intersection of sets **)
  let intersection xs ys = Elt.inter xs ys

  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of xs   *)
  let fold xs f a = Elt.fold f xs a  

  (** Checks if all elements of 'xs' satisfy the predicate p **)
  let forAll xs p = Elt.for_all p xs

  (** Checks if exists an element of 'xs' satisfing p **)
  let exists xs p = Elt.exists p xs
		      
  (** Filters all elements of 'xs' satisfy the predicate p **)
  let filter xs p = Elt.filter p xs
  (***********************************************************************)


  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with the flat type f.                                             **)
  (***********************************************************************) 
  (** Constructs a empty set  **)
  let empty_f = Felt.empty

  (** is 'x' member of 'xs' ? **)
  let isMember_f xs x = Felt.mem x xs

  (** adds 'x' to 'xs' provided that x isn't already **)
  let add_f xs x = Felt.add x xs

  (** converts a set into a list **) 
  let elements_f xs = Felt.elements xs

  (** is xset subset of yset ? **)
  let isSubset_f xs ys = Felt.subset xs ys

  (** are the sets 'xs' and 'ys' equals ? **)
  let isEqual_f xs ys = Felt.equal xs ys

  (** union of sets **)
  let union_f xs ys = Felt.union xs ys
			
  (** intersection of sets **)
  let intersection_f xs ys = Felt.inter xs ys

  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of s   *)
  let fold_f xs f a = Felt.fold f xs a    

  (** Checks if exists an element of 'xs' satisfing p **)
  let exists_f xs p = Felt.exists p xs
			
  (** Checks if all elements of the 'xs' satisfy the predicate p **)
  let forAll_f xs p = Felt.for_all p xs
			
  (** Filters all elements of 'xs' satisfy the predicate p **)
  let filter_f xs p = Felt.filter p xs 
  (***********************************************************************)


  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with the left values.                                             **)
  (***********************************************************************) 
			
  (** adds 'x' to 'xs' provided that x isn't already **)
  let add_l xs x = Lelt.add x xs
		     
  (** Constructs a empty set  **)
  let empty_l = Lelt.empty
		  
  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of xs   *)
  let fold_l xs f a = Lelt.fold f xs a    

  (** Checks if all elements of the 'xs' satisfy the predicate p **)
  let forAll_l xs p = Lelt.for_all p xs
			
  (** Checks if exists an element of 'xs' satisfing p **)
  let exists_l xs p = Lelt.exists p xs 

  (***********************************************************************)

			
  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with the right values.                                             **)
  (***********************************************************************) 
			
  (** adds 'x' to 'xs' provided that x isn't already **)
  let add_r xs x = Relt.add x xs
		     
  (** Constructs an empty set  **)
  let empty_r = Relt.empty

  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of xs   *)
  let fold_r xs f a = Relt.fold f xs a    

  (** Checks if all elements of the 'xs' satisfy the predicate p **)
  let forAll_r xs p = Relt.for_all p xs
			
  (** Checks if exists an element of 'xs' satisfing p **)
  let exists_r xs p = Relt.exists p xs 

  (***********************************************************************)

			
  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with objects.                                                     **)
  (***********************************************************************) 
			
  (** adds 'x' to 'xs' provided that x isn't already **)
  let add_o xs x = Oelt.add x xs
		     
  (** Constructs a empty set  **)
  let empty_o = Oelt.empty

  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of xs   *)
  let fold_o xs f a = Oelt.fold f xs a    

  (** Checks if all elements of the 'xs' satisfy the predicate p **)
  let forAll_o xs p = Oelt.for_all p xs
			
  (** Checks if exists an element of 'xs' satisfing p **)
  let exists_o xs p = Oelt.exists p xs

  (***********************************************************************) 

			
  (***********************************************************************)
  (** Functionalities taken from the standard library Set and related   **)
  (** with threads.                                                     **)
  (***********************************************************************) 
			
  (** adds 'x' to 'xs' provided that x isn't already **)
  let add_t xs x = Telt.add x xs
		     
  (** Constructs a empty set  **)
  let empty_t = Telt.empty

  (** Computes (f xN ... (f x2 (f x1 a))...), **)
  (** where x1 ... xN are the elements of xs   *)
  let fold_t xs f a = Telt.fold f xs a    

  (** Checks if all elements of the 'xs' satisfy the predicate p **)
  let forAll_t xs p = Telt.for_all p xs

  (** Checks if exists an element of 'xs' satisfing p **)
  let exists_t xs p = Telt.exists p xs

  (***********************************************************************) 

			
  (***********************************************************************)
  (************* New functionalities added to the Event Space ************)
  (***********************************************************************)
			
  (** Returns the set of (diferent) events of 
    Event Space 'evs'.
  **)
  let carrier evs = 
    (fold 
       evs
       (function e -> function xs -> match e with (x,y) -> add_f (add_f xs x) y)
       (empty_f)
    )
    
  (*-* Are 'x' and 'y' related in the Event Space 'evs' ? *-*)
  let rec isRelated evs x y =
    x=y 
      ||
      (	exists	
	  evs
	  (	function (w,z) ->
		  if w=x then
		    z=y || (z<>w && isRelated evs z y)	
		  else false
	  )
      )	
	
  (** Is the Event Space 'evs' reflexive ? **)
  (*-* a = (carrier evs) *-*)
  let isReflexive a = true
			
  (** Is the Event Space 'evs' transitive ? **)
  (*-* a = (carrier evs) *-*)
  let isTransitive a = true

  (** Is the Event Space 'evs' antisimetric ? **)
  (*-* a = (carrier evs) *-*)
  let isAntisimetric a = true

  (** Is the Event Space 'evs' total ? **)
  (** 'xs' is subset of events of 'evs', usually xs << (carrier evs) **)
  let isTotal evs xs = true

  (*-* ???
    (	forAll_f
    xs
    (function x ->
    (forAll_f xs (function y ->
    (isRelated evs x y) || (isRelated evs y x))))
    )
    *-*)	
  (***********************************************************************) 
			 


  (***********************************************************************)
  (*                    Some auxiliary functions                         *)
  (***********************************************************************)

  (** display the event space 'evs' **) 
  let display evs pfix swt =
    let r = {prf=pfix; res="\n"} in
    let () = r.res <- r.res ^ r.prf
    and () = r.res <- r.res ^"|--"
    and () = r.prf <- r.prf ^"|  "
    and () = r.res <- r.res ^"EVENT_SPACE" in
    let () =
      (	fold
	  evs
	  (function (e1,e2)-> function a ->
	     r.res <- r.res ^(disp_event_pair r.prf e1 e2 swt)
	  )		
	  ()	
      )
    in r.res 

  (** Retrieves all left values from a = carrier evs **)
  let getLvals a = 
    (	fold_f
	  a
	  ( function e -> function xs -> match e with
		Use(_,l,_) |
		  Assign(_,l,_) |
		    Load(_,l,_) |
		      Store(_,l,_) |
			Read(_,l,_) |
			  Write(_,l,_) -> add_l xs l 
	      | _            -> xs
	  )																
	  (empty_l)
    )

  (** Retrieves all right values from a = carrier evs **)
  let getRvals a = 
    (	fold_f
	  a	
	  ( function e -> function xs -> match e with
		Use(_,_,r) |
		  Assign(_,_,r) |
		    Load(_,_,r) |
		      Store(_,_,r)|
			      Read(_,_,r) |
				Write(_,_,r) -> add_r xs r
	      | _            -> xs 	
	  )															
	  (empty_r)
    )

  (** Retrieves all objects from a = carrier evs **)
  let getObjects a =
    ( fold_f
	a
	( function e -> function xs -> match e with
	      Lock(_,o) |
		Unlock(_,o) -> add_o xs o 
	    | _           -> xs 	
	)															
	(empty_o)
    )
    
  (** Retrieves all objects from a = carrier evs **)
  let getThreads a =
    ( fold_f
	a
	( function e -> function xs -> match e with
	    | Use(th,_,_)
	    | Assign(th,_,_)
	    | Load(th,_,_)
	    | Store(th,_,_)
	    | Read(th,_,_)
	    | Write(th,_,_)
	    | Lock(th,_) 
	    | Unlock(th,_) -> add_t xs th 
	)															
	(empty_t)
    )
    
  (** Retrieves the last Assign event from the Event Space 'evs'. **)
  let getLastAssign evs th =
    let a = carrier evs in
      match
	( fold_f 
	    a 
	    (function e -> function xs ->
	       match e with 
		 | Assign(t,_,_) -> 
		     if t=th then
		       if xs=[] then [e] else if (isRelated evs (List.hd xs) e) then [e] else xs
		     else xs
		 | _ -> xs
	    )
	    []
	)
      with
	|	[]	-> None
	| l::_ 	-> Some l 	
	    
  (** Retrieves the last  event from the Event Space 'evs'. **)
  let getLastStore evs th =
    let a = carrier evs in
      match	
	( fold_f 
	    a 
	    (function e -> function xs ->
	       match e with 
		 | Store(t,_,_) -> 
		     if t=th then 
		       if xs=[] then [e] else if (isRelated evs (List.hd xs) e) then [e] else xs
		     else xs	
		 | _	 -> xs
	    )
	    []
	)
      with
	|	[]	-> None
	| l::_ 	-> Some l 	
	    
  (** Retrieves the last Write events from the Event Space 'evs'. **)
  let getLastWriteList evs =
    let a = carrier evs in
    let f t l =
      (	fold_f 
	  a 
	  (function e -> function xs -> 
	     match e with 
	       | Write(t1,l1,_) -> 
		   if t1=t && l1=l  then
		     if xs=[] then [e] else if (isRelated evs (List.hd xs) e) then [e] else xs
		   else xs
	       | _ -> xs
	  )	
	  []
      ) in
      (	fold_t
	  (getThreads a)
	  ( function t -> function listt ->
	      ( fold_l
		  (getLvals a)
		  ( function l -> function listl ->
		      match f t l with
			| []		-> 	listl
			|	lh::lt ->	lh::listl
		  )
		  []	
	      )@listt	
	  )	
	  []
      )	
      
  (** Retrieves the last Read events from the Event Space 'evs'. **)
  let getLastReadList evs =
    let a = carrier evs in
    let f t l =
      (	fold_f 
	  a 
	  (function e -> function xs -> 
	     match e with 
	       | Read(t1,l1,_) -> 
		   if t1=t && l1=l  then
		     if xs=[] then [e] else if (isRelated evs (List.hd xs) e) then [e] else xs
		   else xs
	       | _ -> xs
	  )	
	  []
      ) in
      (	fold_t
	  (getThreads a)
	  ( function t -> function listt ->
	      ( fold_l
		  (getLvals a)
		  ( function l -> function listl ->
		      match f t l with
			| []		-> 	listl
			|	lh::lt ->	lh::listl
		  )
		  []	
	      )@listt	
	  )	
	  []
      )	
      
  (** computes the number of locks that 'the current thread' 'th' has
    owned on the 'this Thread' 'o': the number of 'Lock(th,o)' without
    matching 'Unlock(th,o)'.
  **)		
  let locks evs th o =
    let a = carrier evs in
      ( fold_f
	  a
	  (function e -> function xs -> 
	     match e with
	       | Lock(th1,o1) -> if th1=th && o1=o then xs+1 else xs
	       | _ -> xs
	  )	
	  0
      )
      (***********************************************************************)
      
      
      
  (***********************************************************************)
  (*         This module presents the rules defining the property        *)
  (*                         "beeing an Event Space"                     *)
  (***********************************************************************)

  (** Predicate returning the set actions performed by the thread 'th' **)
  let alpha th = function 
    | Lock(t,_)
    | Unlock(t,_) 
    | Use(t,_,_) 	
    | Assign(t,_,_) 
    | Load(t,_,_) 	
    | Store(t,_,_)    -> if t=th then true else false

    | Read(_,_,_) 
    | Write(_,_,_)     -> false

  (** Predicate returning the set of actions performed by the Main **)
  (** Memory for any one object 'o' **)
  let beta o = function 
      Lock(_,o1) |
	Unlock(_,o1)  -> if o1=o then true else false
    | _             -> false

  (** Predicate returning the set of actions performed by the Main **)
  (** Memory for any one variable 'l' **)
  let betal l = function 
    | Write(_,l1,_) 
    | Read(_,l1,_)   -> if l1=l then true else false
    | _ -> false
	
  (** The monotone injective partial function implementing 'read_of' **)
  let read_of evs = function
      Load(th,l,r) ->	let (nl,nr) = 
	( fold
	    evs
	    (function e -> function (n1,n2) -> 
	       match e with (x,y) -> 
		 ((if x=Load(th,l,r) then if y=Load(th,l,r) then n1+2 else n1+1 else n1), 
		  (if x=Read(th,l,r) then if y=Read(th,l,r) then n2+2 else n2+1 else n2))
	    )
	    (0,0)
	) in
	if (nr>=nl) &&	(isMember_f (carrier evs) (Read(th,l,r))) 
	then Some (Read(th,l,r)) 
	else None
    | _	-> None	

  (** 'load_of' is the partial inverse function of 'read_of' **)
  let load_of evs = function
      Read(th,l,r) as f ->
	(
	  match read_of evs (Load(th,l,r)) with
	    | Some f1 -> if f1=f then Some (Load(th,l,r)) else None 
	    | _	-> None
	)	
    | _ -> None	


  (** The monotone injective partial function implementing 'store_of' **)
  let store_of evs = function
      Write(th,l,r) ->let (nw,ns) = 
	( fold
	    evs
	    (function e -> function (n1,n2) -> 
	       match e with (x,y) -> 
		 ((if x=Write(th,l,r) then if y=Write(th,l,r) then n1+2 else n1+1 else n1), 
		  (if x=Store(th,l,r) then if y=Store(th,l,r) then n2+2 else n2+1 else n2))
	    )
	    (0,0)
	) in
	if (ns>=nw) &&	(isMember_f (carrier evs) (Store(th,l,r))) 
	then Some (Store(th,l,r)) 
	else None
    | _							-> None	

  (** 'write_of' is the partial inverse function of 'store_of' **)
  let write_of evs = function
    | Store(th,l,r) as f-> 
	(
	  match store_of evs (Write(th,l,r)) with
	    | Some f1 -> if f1=f then Some (Write(th,l,r)) else None 
	    | _	-> None
	)
    | _	-> None	

  (** lock_of **)
  let lock_of evs = function
    | Unlock(th,o) ->	
	if isMember_f (carrier evs) (Lock(th,o))
	then Some (Lock(th,o)) 
	else None
    | _ -> None

  (** **)
  let unlock_of evs = function
    | Lock(th,o) as f ->
	( match lock_of evs (Unlock(th,o)) with
	    | Some f1 -> if f1=f then Some (Unlock(th,o)) else None
	    | _	-> None	
	)
    | _	-> None


  (************************ The rules themselves... ***********************)

  (*-* a =(carrier evs) *-*)
  let rule1 evs a =
    ( forAll_t
	(getThreads a)
	(function th -> isTotal evs (filter_f a (alpha th)))
    ) 

  (*-* a =(carrier evs) *-*)
  let rule2o evs a =
    ( forAll_o
	(getObjects a)
	(function o -> isTotal evs (filter_f a (beta o)))
    )

  (*-* a =(carrier evs) *-*)
  let rule2l evs a =
    ( forAll_l
	(getLvals a)
	(function l -> isTotal evs (filter_f a (betal l)))
    )

  (*-* a =(carrier evs) *-*)
  let rule3 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			(function r ->
			   ( forAll_r
			       rVals
			       (function s ->
				  let eA = Assign(t,l,r) in 
				  let eL = Load(t,l,s) in
				    if (isRelated evs eA eL) then
				      ( exists_r
					  rVals
					  (function u ->
					     let eS = Store(t,l,u)
					     in (isRelated evs eA eS) && (isRelated evs eS eL)
					  )							
				      )
				    else true	
			       )
			   )
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule4 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			(function r ->
			   ( forAll_r
			       rVals
			       (function s ->
				  let eS1 = Store(t,l,r) in 
				  let eS2 = Store(t,l,s) in
				    if not (eS1=eS2) then
				      if(isRelated evs eS1 eS2) then
					(	exists_r
						  rVals
						  (function u ->
						     let eA = Assign(t,l,u)
						     in (isRelated evs eS1 eA) && (isRelated evs eA eS2)
						  )							
					)
				      else true	
				    else true	
			       )
			   )
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule5 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			( function r ->
			    let eU = Use(t,l,r) in 
			      if (isMember_f a eU) then
				(	exists_r
					  rVals
					  ( function s ->
					      let eA = Assign(t,l,s)
					      in (isRelated evs eA eU)
					  )							
				)
				||
				(	exists_r
					  rVals
					  ( function s ->
					      let eL = Load(t,l,s)
					      in (isRelated evs eL eU)
					  )
				)
			      else true	
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule6 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			( function r ->
			    let eS = Store(t,l,r) in 
			      if (isMember_f a eS) then
				(	exists_r
					  rVals
					  ( function s ->
					      let eA = Assign(t,l,s)
					      in (isRelated evs eA eS)
					  )							
				)
			      else true	
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule7 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			(function r ->
			   (	forAll_r
				  rVals
				  (function s ->
				     let eA = Assign(t,l,r) in 
				     let eU = Use(t,l,s) in
				       if not (r=s) then
					 if (isRelated evs eA eU) then
					   ( exists_r
					       rVals
					       (function u ->
						  let eA1 = Assign(t,l,u) 
						  and eL = Load(t,l,u) in
						    ( ( (isRelated evs eA eA1) && 
							(isRelated evs eA1 eU) &&
							not(eA = eA1)
						      )
						      ||
						      ( (isRelated evs eA eL) &&
							(isRelated evs eL eU)
						      )
						    )
					       )
					   )
					 else true	
				       else true	
				  )
			   )
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule8 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			(function r ->
			   (	forAll_r
				  rVals
				  (function s ->
				     let eL = Load(t,l,r) in 
				     let eU = Use(t,l,s) in
				       if not (r=s) then
					 if (isRelated evs eL eU) then
					   ( exists_r
					       rVals
					       (function u ->
						  let eA = Assign(t,l,u) 
						  and eL1 = Load(t,l,u) in
						    ( ( (isRelated evs eL eA) && 
							(isRelated evs eA eU) 
						      )
						      ||
						      ( (isRelated evs eL eL1) &&
							(isRelated evs eL1 eU)
						      )
						    )
					       )
					   )
					 else true	
				       else true	
				  )
			   )
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule9 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			(function r ->
			   ( forAll_r
			       rVals
			       (function s ->
				  let eA = Assign(t,l,r) in 
				  let eS = Store(t,l,s) in
				    if not (r=s) then
				      if (isRelated evs eA eS) then
					( exists_r
					    rVals
					    (function u ->
					       let eA1 = Assign(t,l,u) in
						 (isRelated evs eA eA1) &&
						 (isRelated evs eA1 eS) &&
						 not (eA = eA1)
					    )
					)
				      else true	
				    else true	
			       )
			   )
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule10 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			( function r ->
			    let eL = Load(t,l,r) in
			      if (isMember_f a eL) then
				( let eR = Read(t,l,r) in
				    (read_of evs eL) = (Some eR) &&
						       (isRelated evs eR eL)	
				)	 
			      else true
			)
		    )
	       )
	   )
	)
    )
    
  (*-* a =(carrier evs) *-*)
  let rule11 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  let rVals = (getRvals a) in
		    ( forAll_r
			rVals
			( function r ->
			    let eW = Write(t,l,r) in
			      if (isMember_f a eW) then
				(
				  let eS = Store(t,l,r)
				  in (store_of evs eW) = (Some eS) &&
							 (isRelated evs eS eW)	
				)	 
			      else true
			)
		    )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule12 evs a =	
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  ( forAll_r
		      (getRvals a)
		      (function r ->
			 if isRelated evs (Store(t,l,r)) (Load(t,l,r)) then
			   (
			     match (write_of evs (Store(t,l,r))), (read_of evs (Load(t,l,r))) with
			       | (Some f1,Some f2) -> isRelated evs f1 f2
			       | _ -> false	
			   )
			 else true
		      )
		  )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule13 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_o
	       (getObjects a)
	       (function o ->
		  if isMember_f a (Unlock(t,o)) then
		    match lock_of evs (Unlock(t,o)) with
			Some f -> isRelated evs f (Unlock(t,o))
		      | _ -> false
		  else true
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule14 evs a =
    let tvals = (getThreads a) in
      (	forAll_t
	  tvals
	  ( function t ->
	      ( forAll_t
		  tvals
		  ( function u ->
		      ( forAll_o
			  (getObjects a)
			  ( function o ->
			      if not(t=u) then
				if isRelated evs (Lock(t,o)) (Lock(u,o)) then 
				  match unlock_of evs (Lock(t,o)) with
				      Some f ->	isRelated evs f (Lock(u,o))
				    | _ -> false	
				else true
			      else true
			  )
		      )
		  )
	      )
	  )
      )
      
  (*-* a =(carrier evs) *-*)
  let rule15 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  ( forAll_r
		      (getRvals a)
		      (function r ->
			 ( forAll_o
			     (getObjects a)
			     ( function o ->
				 if isRelated evs (Assign(t,l,r)) (Unlock(t,o)) then
				   match store_of evs (Write(t,l,r)) with
				       Some f -> (isRelated evs (Assign(t,l,r)) f ) &&
					 (isRelated evs f (Write(t,l,r))) &&
					 (isRelated evs (Write(t,l,r)) (Unlock(t,o)))
				     | None -> false						
				 else true
			     )
			 )
		      )
		  )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule16 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  ( forAll_r
		      (getRvals a)
		      (function r ->
			 ( forAll_o
			     (getObjects a)
			     ( function o ->
				 if isRelated evs (Lock(t,o)) (Use(t,l,r)) then
				   match read_of evs (Load(t,l,r)) with
				     |	Some f -> ( (isRelated evs (Lock(t,o))(Assign(t,l,r))) &&
						    (isRelated evs (Assign(t,l,r))(Use(t,l,r)))
						  ) 
					  ||
					  ( (isRelated evs (Lock(t,o)) f) &&
					    (isRelated evs f (Load(t,l,r))) &&
					    (isRelated evs (Load(t,l,r))(Use(t,l,r)))
					  )
				     | None -> false	
				 else true	
			     )
			 )
		      )
		  )
	       )
	   )
	)
    )

  (*-* a =(carrier evs) *-*)
  let rule17 evs a =
    ( forAll_t
	(getThreads a)
	(function t ->
	   ( forAll_l
	       (getLvals a)
	       (function l ->
		  ( forAll_r
		      (getRvals a)
		      (function r ->
			 ( forAll_o
			     (getObjects a)
			     ( function o ->
				 if isRelated evs (Lock(t,o)) (Store(t,l,r)) then
				   (isRelated evs (Lock(t,o)) (Assign(t,l,r))) &&
				   (isRelated evs (Assign(t,l,r)) (Store(t,l,r)))
				 else true	
			     )
			 )
		      )
		  )
	       )
	   )
	)
    )

  (* is 'evs' an Event Space ? *)
  let isEventSpace evs a =
    let gaga1 = (rule1 evs a) in
    let gaga2l = (rule2l evs a) in
    let gaga2o = (rule2o evs a) in
    let gaga3 = (rule3 evs a) in
    let gaga4 = (rule4 evs a) in
    let gaga5 = (rule5 evs a) in
    let gaga6 = (rule6 evs a) in
    let gaga7 = (rule7 evs a) in
    let gaga8 = (rule8 evs a) in
    let gaga9 = (rule9 evs a) in
    let gaga10 = (rule10 evs a) in
    let gaga11 = (rule11 evs a) in
    let gaga12 = (rule12 evs a) in
    let gaga13 = (rule13 evs a) in
    let gaga14 = (rule14 evs a) in
    let gaga15 = (rule15 evs a) in
    let gaga16 = (rule16 evs a) in
    let gaga17 = (rule17 evs a) in
    let gagar = (isReflexive a) in
    let gagat = (isTransitive a) in
    let gagaa = (isAntisimetric a) in
      gaga1 && gaga2l && gaga2o && gaga3 && gaga4 && gaga5 && gaga6 && gaga7 && 
      gaga8 && gaga9 && gaga10 && gaga11 && gaga12 && gaga13 && gaga14 && gaga15 &&
      gaga16 && gaga17 && gagar && gagat && gagaa

  (* does '_X' extends '_Y' ? *)
  (*-*   let extends _X _Y = *-*)
  (*-*     let _x = carrier _X *-*)
  (*-*     and	_y = carrier _Y in *-*)
  (*-*       (isSubset_f _y _x) && *-*)
  (*-*      (isSubset _Y _X) && *-*)
  (*-*       ( forAll_f *-*)
  (*-* 	  _y *-*)
  (*-* 	  ( function y1 -> *-*)
  (*-* 	      (	forAll_f *-*)
  (*-* 		  _y *-*)
  (*-* 		  ( *-*)
  (*-* 		    function y2 -> not(isRelated _Y y1 y2) || (isRelated _X y1 y2) *-*)
  (*-* 		  ) *-*)
  (*-* 	      ) *-*)
  (*-* 	  ) *-*)
  (*-*       ) *-*)
  (*-*    ( forAll_f *-*) 
  (*-* 	  _y *-*)
  (*-* 	  ( function x -> *-*)
  (*-* 	      ( forAll_f *-*)
  (*-* 		  _y *-*)
  (*-* 		  ( function y -> (not(isRelated _X x y)||(isRelated _Y x y)) && (not(isRelated _Y x y)||(isRelated _X x y))) *-*)
  (*-* 	      ) *-*)
  (*-* 	  ) *-*)
  (*-*       ) *-*)
      
  (*-* Extending 'evs' with the event 'x_' *-*)
  (* Given the event 'x_', this function returns a list of
     candidates to be Event Spaces. These candidateds are calculated
     as the Event Space 'evs' more some associations between 'x_' and 
     events of the Event Space 'evs'.
  *)
  (*-* a = carrier evs *-*)
  let getRsCandidates evs a x_ =
    let b = a in
      (*-* 's' contains all maximal elements of 'carrier evs' *-*)
    let s = (	filter_f a (function x -> (forAll_f b (function y -> (if isRelated evs x y then x=y else true))))) in
      (	fold_f 
	  s 
	  (function x -> function ls -> 
	     (*-* by default, Event spaces are reflexive and transitive *-*)
	     (*-* reflexivity is ensured adding the pair (x_,x_)	*-*)
	     if (isRelated evs x_ x) then 
	       ls
	     else (add (add evs (x,x_)) (x_,x_))::ls
	  ) 
	  []
      )@[(add evs (x_,x_))]


  (*-*    ( forAll_f *-*) 
  (*-* 	  _y *-*)
  (*-* 	  ( function y1 -> *-*)
  (*-* 	      (	forAll_f *-*)
  (*-* 		  _y *-*)
  (*-* 		  ( *-*)
  (*-* 		    function y2 -> not(isRelated _Y y1 y2) || (isRelated _X y1 y2) *-*)
  (*-* 		  ) *-*)
  (*-* 	      ) *-*)
  (*-* 	  ) *-*)



  (* returns the set of possible Event Spaces constructed from 
     the Event Space 'evs' and the event 'x_'.
  *)
  (*-* a = carrier evs *-*)
  let adjointSet evs a x_ =
	  ( List.filter 
	      ( function r -> 
		  let b = (carrier r) in
		    (*-* 1. it's implied by construction of getRsCandidates *-*)
		    (*-* isEqual_f b (add_f a x_) && *-*)
		    (*-* 2. *-*) 
		    (*-* (forAll_f a (function -> x (forAll_f a -> (function y -> (not(isRelated evs x y)||(isRelated r x y)) && 
								 (not(isRelated r x y)||(isRelated evs x y)) )))) && *-*) 
		    (*-* 3. 4. 5. are implied by construction of getRsCandidates *-*)
		    (*-* 6. *-*)
		    isEventSpace r b
	      )								
	      (getRsCandidates evs a x_)
	    )

  (** evs [+] x_ **)
  let plusE evs x_ =
    let a = (carrier evs) in
      match (adjointSet evs a x_) with
    	| [] -> if (isMember_f (carrier evs) x_) then Some evs else None
  	| l -> Some (List.hd l) (*-* ??? it ought be generated radomically *-*)

	    
  (********************************************************************)	
  (************ Some additional properties of Event Spaces ************)	
  (********************************************************************)	

  (** establishes whether the event space 'evs' is complete.         **) 
  (** An event space is complete iff r:Read(th,l,v) > l=load_of(r)   **)
  (** and s:Store(th,l,v) > w=write_of(s), for all r,s.        	     **)
  let isComplete evs =
    let a = carrier evs in
      (	forAll_f
	  a
	  (function Read(th,l,v) -> 
	     begin match load_of evs (Read(th,l,v)) with
	       | None -> false
	       | Some lf -> isRelated evs (Read(th,l,v)) lf
	     end
	     | Store(th,l,v) -> 
		 begin match write_of evs (Store(th,l,v)) with
		   | None -> false
		   | Some sf -> isRelated evs (Store(th,l,v)) sf
		 end
	     | _ -> true
	  )			 
      )
      (************************************************************)	

end ;;

