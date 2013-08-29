all:lex yacc parser

lex:spec.lex
	mllex spec.lex

yacc:spec.grm
	mlyacc spec.grm

parser:
	mlton -output spec sources.mlb

clean:
	rm spec.lex.sml spec.grm.sml spec
