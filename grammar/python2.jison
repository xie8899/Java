
/*jison for python 2.7*/

%%
single_input
	:	NEWLINE
	|	simple_stmt
	|	compound_stmt NEWLINE
	;

file_input
	:	(NEWLINE | stmt)* ENDMARKER
	;

eval_input
	:	testlist NEWLINE* ENDMARKER
	;

decorator
	:	AT dotted_name [ LPAREN [arglist] RPAREN ] NEWLINE
	;

decorators
	:	decorator+
	;

decorated
	:	decorators (classdef | funcdef)
	;

funcdef
	:	DEF Identifier parameters COLON suite
	;

parameters
	: LPAREN [varargslist] RPAREN
	;

varargslist
	:	((fpdef [ASSIGN test] COMMA)*(MUL Identifier [COMMA DOUBLEMUL Identifier] | DOUBLEMUL Identifier) | fpdef [ASSIGN test] (COMMA fpdef [ASSIGN test])* [COMMA])
	;

fpdef
	: Identifier
	| LPAREN fplist RPAREN
	;

fplist
	:	fpdef (COMMA fpdef)* [COMMA]
	;

stmt
	:	simple_stmt
	|	compound_stmt
	;

simple_stmt
	:	small_stmt (SEMI small_stmt)* [SEMI] NEWLINE

small_stmt
	:	(expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | exec_stmt | assert_stmt)
	;

expr_stmt
	:	testlist (augassign (yield_expr|testlist) | (ASSIGN (yield_expr|testlist))*)
	;

augassign
	:	(ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN | AND_ASSIGN | OR_ASSIGN | XOR_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN | DOUBLEMUL_ASSIGN | DOUBLEDIV_ASSIGN)

/*For normal assignments, additional restrictions enforced by the interpreter*/
print_stmt
	:	PRINT ( [ test (COMMA test)* [COMMA] ] | RSHIFT test [ (COMMA test)+ [COMMA] ] )
	;

del_stmt
	:	DEL exprlist
	;

pass_stmt
	:	PASS
	;

flow_stmt
	:	break_stmt
	|	continue_stmt
	|	return_stmt
	|	raise_stmt
	|	yield_stmt
	;

break_stmt
	:	BREAK
	;

continue_stmt
	:	CONTINUE
	;

return_stmt
	:	RETURN [testlist]
	;

yield_stmt
	:	yield_expr
	;

raise_stmt
	:	RAISE [test [COMMA test [COMMA test]]]
	;

import_stmt
	:	import_name
	|	import_from
	;

import_name
	:	IMPORT dotted_as_names
	;

import_from
	:	(FROM (DOT* dotted_name | DOT+) IMPORT (MUL | LPAREN import_as_names RPAREN | import_as_names))
	;

import_as_name
	:	Identifier [AS Identifier]
	;

dotted_as_name
	:	dotted_name [AS Identifier]
	;

import_as_names
	:	import_as_name (COMMA import_as_name)* [COMMA]
	;

dotted_as_names
	:	dotted_as_name (COMMA dotted_as_name)*
	;

dotted_name
	:	Identifier (DOT Identifier)*
	;

global_stmt
	:	GLOBAL Identifier (COMMA Identifier)*
	;

exec_stmt
	:	EXEC expr [IN test [COMMA test]]
	;

assert_stmt
	:	ASSERT test [COMMA test]
	;

compound_stmt
	:	if_stmt
	|	while_stmt
	|	for_stmt
	|	try_stmt
	|	with_stmt
	|	funcdef
	|	classdef
	|	decorated
	;

if_stmt
	:	IF test COLON suite (ELIF test COLON suite)* [ELSE COLON suite]
	;

while_stmt
	:	WHILE test COLON suite [ELSE COLON suite]
	;

for_stmt
	:	FOR exprlist IN testlist COLON suite [ELSE COLON suite]
	;

try_stmt
	:	(TRY COLON suite
		   ((except_clause COLON suite)+
			[ELSE COLON suite]
			[FINALLY COLON suite] |
		   FINALLY COLON suite))
	;

with_stmt
	:	WITH with_item (COMMA with_item)*  COLON suite
	;

with_item
	:	test [AS expr]
	;

/* NB compile.c makes sure that the default except clause is last */
except_clause
	:	EXCEPT [test [(AS | COMMA) test]]
	;

suite
	:	simple_stmt | NEWLINE INDENT stmt+ DEDENT
	;

/*
 Backward compatibility cruft to support:
 [ x for x in lambda: True, lambda: False if x() ]
 even while also allowing:
 lambda x: 5 if x else 2
 (But not a mix of the two)
*/
testlist_safe
	:	old_test [(COMMA old_test)+ [COMMA]]
	;

old_test
	:	or_test
	|	old_lambdef
	;

old_lambdef
	:	LAMBDA [varargslist] COLON old_test
	;

test
	:	or_test [IF or_test ELSE test]
	|	lambdef
	;

or_test
	:	and_test (OR and_test)*
	;

and_test
	:	not_test (AND not_test)*
	;

not_test
	:	NOT not_test
	|	comparison
	;

comparison
	:	expr (comp_op expr)*
	;

comp_op
	:	LT
	|	GT
	|	EQUAL
	|	GE
	|	LE
	|	NOTEQUAL
	|	NOTEQUAL
	|	IN
	|	NOT IN
	|	IS
	|	IS NOT
	;

expr
	:	xor_expr (BITOR xor_expr)*
	;

xor_expr
	:	and_expr (CARET and_expr)*
	;

and_expr
	:	shift_expr (BITAND shift_expr)*
	;

shift_expr
	:	arith_expr ((LSHIFT|RSHIFT) arith_expr)*
	;

arith_expr
	:	term ((ADD|SUB) term)*
	;

term
	:	factor ((MUL|DIV|MOD|INTEGER_DIV) factor)*
	;

factor
	:	(ADD|SUB|TILDE) factor
	|	power
	;

power
	:	atom trailer* [DOUBLEMUL factor]
	;

atom
	:	(LPAREN [yield_expr|testlist_comp] RPAREN |
	   LBRACK [listmaker] RBRACK |
	   LBRACE [dictorsetmaker] RBRACE |
	   DOT2 testlist1 DOT2 |
	   Identifier | NUMBER | STRING+)
	;

listmaker
	:	test ( list_for | (COMMA test)* [COMMA] )
	;

testlist_comp
	:	test ( comp_for | (COMMA test)* [COMMA] )
	;

lambdef
	:	LAMBDA [varargslist] COLON test
	;

trailer
	:	LPAREN [arglist] RPAREN
	|	LBRACK subscriptlist RBRACK
	|	DOT Identifier
	;

subscriptlist
	:	subscript (COMMA subscript)* [COMMA]
	;

subscript
	:	DOT DOT DOT
	|	test
	|	[test] COLON [test] [sliceop]
	;

sliceop
	:	COLON [test]
	;

exprlist
	:	expr (COMMA expr)* [COMMA]
	;

testlist
	:	test (COMMA test)* [COMMA]
	;

dictorsetmaker
	:	( (test COLON test (comp_for | (COMMA test COLON test)* [COMMA])) |
				  (test (comp_for | (COMMA test)* [COMMA])) )
	;

classdef
	:	CLASS Identifier [LPAREN [testlist] RPAREN] COLON suite
	;

arglist
	:	(argument COMMA)* (argument [COMMA]
						 |MUL test (COMMA argument)* [COMMA DOUBLEMUL test]
						 |DOUBLEMUL test)
	;

/*
 The reason that keywords are test nodes instead of Identifier is that using Identifier
 results in an ambiguity. ast.c makes sure it's a Identifier.
*/

argument
	:	test [comp_for]
	|	test ASSIGN test
	;

list_iter
	:	list_for
	|	list_if
	;

list_for
	:	FOR exprlist IN testlist_safe [list_iter]
	;

list_if
	:	IF old_test [list_iter]
	;

comp_iter
	:	comp_for
	|	comp_if
	;

comp_for
	:	FOR exprlist IN or_test [comp_iter]
	;

comp_if
	:	IF old_test [comp_iter]
	;

testlist1
	:	test (COMMA test)*
	;

/* not used in grammar, but may appear in "node" passed from Parser to Compiler */
encoding_decl
	:	Identifier
	;

yield_expr
	:	YIELD [testlist]
	;

%%
