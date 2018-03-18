////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Antlr4 lexer grammar for Alogic
////////////////////////////////////////////////////////////////////////////////

lexer grammar AlogicLexer;

channels {
  WHITESPACE,
  COMMENT
}

fragment LCMT: '//' .*? NL;                 // Line comment
fragment BCMT: '/*'  .*? '*/';              // Block comment
CMT: (LCMT | BCMT) -> channel(COMMENT) ;    // Any comment

UINTTYPE: 'u' [0-9]+;

INTTYPE: 'i' [0-9]+;


// Literals
STRING: '"' ~["]* '"';

fragment DECIMALDIGIT: [0-9] ;
fragment DECIMALVALUE
  : DECIMALDIGIT
  | DECIMALDIGIT (DECIMALDIGIT|'_')* DECIMALDIGIT
  ;

fragment HEXDIGIT: (DECIMALDIGIT|[a-fA-F]) ;
fragment HEXVALUE
  : HEXDIGIT
  | HEXDIGIT (HEXDIGIT|'_')* HEXDIGIT
  ;

UNSIZEDINT
  :                          DECIMALVALUE 's'?
  |               '\'' [bdh] HEXVALUE     's'?
  ;
SIZEDINT
  : DECIMALDIGIT+ '\'' [bdh] HEXVALUE     's'?
  ;

ATID    : '@' SIMPLEID  ;

DOLLARID: '$' SIMPLEID;

DOLLAR: '$';

GOESTO: '->';


MUL:        '*'   ;
DIV:        '/'   ;
MOD:        '%'   ;

PLUS:       '+'   ;
MINUS:      '-'   ;

NOT:        '~'   ;
AND:        '&'   ;
OR:         '|'   ;
XOR:        '^'   ;

LSHIFT:     '<<'  ;
URSHIFT:    '>>'  ;
SRSHIFT:    '>>>' ;
SLSHIFT:    '<<<' ;

PLING:      '!'   ;
ANDAND:     '&&'  ;
OROR:       '||'  ;

EQ:         '=='  ;
NE:         '!='  ;

GT:         '>'   ;
GE:         '>='  ;
LE:         '<='  ;
LT:         '<'   ;


QUESTIONMARK: '?';

COMMA:      ','   ;
SEMICOLON:  ';'   ;

PLUSPLUS:   '++'  ;
MINUSMINUS: '--'  ;
EQUALS:     '='   ;
ASSIGNOP
  : '*='
  | '/='
  | '%='
  | '+='
  | '-='
  | '<<='
  | '>>='
  | '>>>='
  | '<<<='
  | '&='
  | '|='
  | '^='
  ;

DOT: '.';

LEFTCURLY:    '{' ;
RIGHTCURLY:   '}' ;
LEFTSQUARE:   '[' ;
RIGHTSQUARE:  ']' ;
LEFTBRACKET:  '(' ;
RIGHTBRACKET: ')' ;

COLON: ':';
MINUSCOLON: '-:';
PLUSCOLON: '+:';

// Keywords
FSM     : 'fsm';
NETWORK : 'network';
PIPELINE: 'pipeline';
TYPEDEF : 'typedef';
STRUCT  : 'struct';
IN      : 'in';
OUT     : 'out';
CONST   : 'const' ;
PARAM   : 'param' ;
FENCE   : 'fence' ;
TRUE    : 'true' ;
FALSE   : 'false' ;
VOID    : 'void' ;
BOOL    : 'bool';
UINT    : 'uint' ;
INT     : 'int' ;
LOOP    : 'loop' ;
WHILE   : 'while' ;
DO      : 'do' ;
FOR     : 'for' ;
IF      : 'if' ;
GOTO    : 'goto' ;
ELSE    : 'else' ;
BREAK   : 'break' ;
RETURN  : 'return' ;
CASE    : 'case' ;
DEFAULT : 'default' ;
VERILOG : 'verilog' ;
NEW     : 'new' ;
LET     : 'let' ;
ENTITY  : 'entity' ;
STATIC  : 'static' ;
STALL   : 'stall' ;
COMB    : 'comb'  ;
STACK   : 'stack' ;

SYNC        : 'sync';
SYNC_READY  : 'sync' (WS|CMT)* 'ready';
SYNC_ACCEPT : 'sync' (WS|CMT)* 'accept';

WIRE        : 'wire';
BUBBLE      : 'bubble';
FSLICE      : 'fslice';
BSLICE      : 'bslice';

VERILOGFUNC: 'void' (WS|CMT)* 'verilog' (WS|CMT)* '(' (WS|CMT)* ')' (WS|CMT)* -> pushMode(VERBATIMMODE);

VERBATIM: 'verbatim' -> pushMode(VERBATIMLANGMODE);

IDENTIFIER: SIMPLEID;

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;

fragment NL
  : '\r'? '\n'
  ;

WS
  : ([ \t] | NL)+ -> channel(WHITESPACE)
  ;

ERRORCHAR : . ;

////////////////////////////////////////////////////////////////////////////////

mode VERBATIMLANGMODE;

VERBATIMENTITY: ENTITY -> type(ENTITY), popMode;

VERBATIMLANGWS: WS -> channel(WHITESPACE);

VERBATIMIDENTIFIER: IDENTIFIER -> type(IDENTIFIER), Mode(VERBATIMMODE);

////////////////////////////////////////////////////////////////////////////////

mode VERBATIMMODE;

VERBATIMWS: WS -> channel(WHITESPACE);

VERBATIMBODY: '{' ( VERBATIMBODY | ~[{}] )* '}'  -> popMode;

