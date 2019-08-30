open Javasyntax ;;
open Display;;

let greater x y = 
  match x,y with
    | (Ival(0),Ival(_)) -> 	Bval(false)
    | (Ival(1),Ival(0)) ->	Bval(true)
    | (Ival(1),Ival(_)) ->	Bval(false)	
    | (Ival(2),Ival(0)) -> 	Bval(true)
    | (Ival(2),Ival(1)) -> 	Bval(true)
    | (Ival(2),Ival(_)) -> 	Bval(false)
    | (Ival(3),Ival(0)) -> 	Bval(true)
    | (Ival(3),Ival(1)) -> 	Bval(true)
    | (Ival(3),Ival(2)) -> 	Bval(true)
    | (Ival(3),Ival(_)) -> 	Bval(false)
    | (Ival(4),Ival(0)) -> 	Bval(true)
    | (Ival(4),Ival(1)) -> 	Bval(true)
    | (Ival(4),Ival(2)) -> 	Bval(true)
    | (Ival(4),Ival(3)) -> 	Bval(true)
    | (Ival(4),Ival(_)) -> 	Bval(false)
    | (Ival(5),Ival(0)) -> 	Bval(true)
    | (Ival(5),Ival(1)) -> 	Bval(true)
    | (Ival(5),Ival(2)) -> 	Bval(true)
    | (Ival(5),Ival(3)) -> 	Bval(true)
    | (Ival(5),Ival(4)) -> 	Bval(true)
    | (Ival(5),Ival(_)) -> 	Bval(false)
    | (Ival(6),Ival(0)) -> 	Bval(true)
    | (Ival(6),Ival(1)) -> 	Bval(true)
    | (Ival(6),Ival(2)) -> 	Bval(true)
    | (Ival(6),Ival(3)) -> 	Bval(true)
    | (Ival(6),Ival(4)) -> 	Bval(true)
    | (Ival(6),Ival(5)) -> 	Bval(true)
    | (Ival(6),Ival(_)) -> 	Bval(false)
    | (_,_) -> 	Bval(false)

let minusbinop x y =
  match x with
    | Ival(1) -> Ival(0)
    | Ival(2) -> Ival(1)
    | Ival(3) -> Ival(2)
    | Ival(4) -> Ival(3)
    | Ival(5) -> Ival(4)
    | Ival(6) -> Ival(5)
    | Ival(_) -> Ival(0)
    | _  -> Ival(0)

let swap = 
  CompilC( Package(""),
	   [],
	   Class("Swap",Extends("Object"),[],
		 [	FldMem(PrimType(IntType),"a");
			FldMem(PrimType(IntType),"b");

			ConstrMem( CId("Swap",[]),
				   BlockIt([	ExpStmt(Ass(Field(This,FId("a",PrimType(IntType))),Lit(IntLit(7))));
						ExpStmt(Ass(Field(This,FId("b",PrimType(IntType))),Lit(IntLit(11))));
					   ],
					   Stack_of_env.createEnv()
					  ),	
				   []
				 );
			
			MethMem( MId("p",Void,[]),
				 BlockIt([	
						(*-*ExpStmt(Ass(Var("a"),Acc(Var("b"))));*-*)
				ExpStmt(Ass(Field(This,FId("a",PrimType(IntType))),Acc(Var("b"))));
			(*-*			SyncStmt(This,BlockIt([ExpStmt(Ass(Field(This,FId("a",PrimType(IntType))),Acc(Var("b"))));],Stack_of_env.createEnv())); *-*)
					 ],
					 Stack_of_env.createEnv()),
				 []
			       );

			MethMem( MId("q",Void,[]),
				 BlockIt([
						(*-*ExpStmt(Ass(Var("b"),Acc(Var("a"))));*-*)
				ExpStmt(Ass(Field(This,FId("b",PrimType(IntType))),Acc(Var("a"))));
				(*-*		SyncStmt(This,BlockIt([ExpStmt(Ass(Field(This,FId("b",PrimType(IntType))),Acc(Var("a"))));],Stack_of_env.createEnv())); *-*)
					 ],
					 Stack_of_env.createEnv()),
				 []
			       );

			MethMem( MId("main",Void,[ClType("String");]),
				 BlockIt([	VarDeclStmtE(ClType("Swap"),"s",StmtExp(NewC("Swap",[])));
						VarDeclStmtE(ClType("Thread1"),"thp",StmtExp(NewC("Thread1",[Acc(Var("s"))])));
						VarDeclStmtE(ClType("Thread2"),"thq",StmtExp(NewC("Thread2",[Acc(Var("s"))])));
						ExpStmt(MCall(Acc(Var("thp")),MCId(MId("start",Void,[])),[]));
						ExpStmt(MCall(Acc(Var("thq")),MCId(MId("start",Void,[])),[]));
					 ],Stack_of_env.createEnv()),
				 []
			       );
		 ]
		)
	 );;	

let myrunnerp = CompilC( Package(""),
			 [],
			 Class("Thread1",Extends("Thread"),[],
			       [
				 FldMem(ClType("Swap"),"s1");
				 
				 ConstrMem( CId("Thread1",[ClType("Swap")]),
					    BlockIt([
						      ExpStmt(Ass(Field(This,FId("s1",ClType("Swap"))),Acc(Var("s"))));
						      (*-*ExpStmt(Ass(Var("s1"),Acc(Var("s"))));*-*)
						    ],
						    Stack_of_env.createEnv()
						   ),
					    ["s";]
					  );

				 MethMem( MId("run",Void,[]),
					  BlockIt([
						    ExpStmt(MCall(Acc(Field(This,FId("s1",ClType("Swap")))),MCId(MId("p",Void,[])),[]));
						    (*-*ExpStmt(MCall(Acc(Var("s1")),MCId(MId("p",Void,[])),[]));*-*)
						  ],
						  Stack_of_env.createEnv()),
					  []
					);					
			       ]
			      )
		       );;	

let myrunnerq = CompilC( Package(""),
			 [],
			 Class("Thread2",Extends("Thread"),[],
			       [
				 FldMem(ClType("Swap"),"s2");
				 ConstrMem( CId("Thread2",[ClType("Swap")]),
					    BlockIt([
						      ExpStmt(Ass(Field(This,FId("s2",ClType("Swap"))),Acc(Var("s"))));
						      (*-*ExpStmt(Ass(Var("s2"),Acc(Var("s"))));*-*)
						    ],	
						    Stack_of_env.createEnv()
						   ),
					    ["s";]
					  );				
				 MethMem( MId("run",Void,[]),
					  BlockIt([	
						    ExpStmt(MCall(Acc(Field(This,FId("s2",ClType("Swap")))),MCId(MId("q",Void,[])),[]));
						    (*-*ExpStmt(MCall(Acc(Var("s2")),MCId(MId("q",Void,[])),[]));*-*)
						  ],
						  Stack_of_env.createEnv()),
					  []
					);					
			       ]
			      )
		       );;	


module Oper = Operational.Make;;

let (mt,cnf) = 	Oper.run swap [myrunnerp;myrunnerq] 
		"traceOfswap.evsp" 
		Light
;;
