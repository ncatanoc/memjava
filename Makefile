#*********************************************************************
# 
# Néstor CATAÑO COLLAZOS
# INRIA, France
# 2004, route des Lucioles - B.P. 93
# 06902 Sophia-Antipois Cedex(France)
#
# E-mail Nestor.Catano@sophia.inria.fr
# http://www-sop.inria.fr/lemme/verificard/modifSpec/index.html
#
#*********************************************************************

OCAMLC   = ocamlc
OCAMLD	 = ocamldebug
OCAMLR   = ocamlrun
MAKE     = make
DELETE   = rm -f
PRINT    = @echo
CLEAR    = clear

OPTIONS = \
	-g -c

FILES = javasyntax.ml \
	display.ml \
	event_space.ml \
	stack_of_env.ml \
	prelude.ml \
	store.ml \
	operational.ml \

COMPILE = $(subst .ml,.cmo,$(FILES))


#***************************************************************************
#
#  Rules and dependencies.
#
#  target  - the parameter given to make: What to build
#  depends - file or other targets target depends on
#  rule    - how to create target
#  $(VAR)  - environment variable or variable defined above
#  $@      - Current target
#  $*      - Current target without extension
#  $<      - Current dependency
#
#***************************************************************************

compile-all: $(COMPILE)

javasyntax.cmo: javasyntax.ml
	$(OCAMLC) $(OPTIONS) $<

display.cmo: display.ml \
	javasyntax.cmo
	$(OCAMLC) $(OPTIONS) $<

stack_of_env.cmi: stack_of_env.mli \
	javasyntax.cmo \
	display.cmo
	$(OCAMLC) $(OPTIONS) $<

stack_of_env.cmo: stack_of_env.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmi
	$(OCAMLC) $(OPTIONS) $<

prelude.cmi: prelude.mli \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo
	$(OCAMLC) $(OPTIONS) $<

prelude.cmo: prelude.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	prelude.cmi
	$(OCAMLC) $(OPTIONS) $<

store.cmi: store.mli \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	prelude.cmo
	$(OCAMLC) $(OPTIONS) $<

store.cmo: store.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	store.cmi
	$(OCAMLC) $(OPTIONS) $<

event_space.cmo: event_space.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	store.cmo 
	$(OCAMLC) $(OPTIONS) $<

operational.cmo: operational.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	store.cmo \
	event_space.cmo
	$(OCAMLC) $(OPTIONS) $<

compile-test: test.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	store.cmo \
	event_space.cmo \
	operational.cmo
	$(OCAMLC) $(COMPILE) -g -o test.sh $<

compile-swap: swap.ml \
	javasyntax.cmo \
	display.cmo \
	stack_of_env.cmo \
	store.cmo \
	event_space.cmo \
	operational.cmo
	$(OCAMLC) $(COMPILE) -g -o swap.sh $<

run-swap: swap.sh
	./$<

run: test.sh
	./$<

clean: 
	$(PRINT) ... cleaning all
	$(DELETE) *.cm*
	$(DELETE) *.out
	$(DELETE) *~
	$(CLEAR)

debug-test: test.sh
	$(OCAMLD) $<

debug-swap: swap.sh
	$(OCAMLD) $<
