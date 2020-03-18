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
fragment DECIMALVALUE: DECIMALDIGIT ('_'? DECIMALDIGIT)* ;

fragment HEXDIGIT: [0-9a-fA-F] ;
fragment HEXVALUE: HEXDIGIT ('_'? HEXDIGIT )* ;

UNSIZEDINT
  : DECIMALVALUE [us]?
  | '0' [bodx] HEXVALUE [us]?
  ;

SIZEDINT: DECIMALDIGIT+ '\'' 's'? [bdh] HEXVALUE ;

ATID    : '@' SIMPLEID  ;

DOLLARID: '$' SIMPLEID  ;

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

TICK:       '\''  ;

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

LEFTATTR:  '(*' ;
RIGHTATTR: '*)' ;

COLON: ':';
MINUSCOLON: '-:';
PLUSCOLON: '+:';

HASH: '#' ;

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
CONTINUE: 'continue' ;
RETURN  : 'return' ;
CASE    : 'case' ;
DEFAULT : 'default' ;
NEW     : 'new' ;
LET     : 'let' ;
ENTITY  : 'entity' ;
STATIC  : 'static' ;
STALL   : 'stall' ;
COMB    : 'comb'  ;
STACK   : 'stack' ;
SRAM    : 'sram' ;
REG     : 'reg' ;
SIGNED  : 'signed' ;
UNSIGNED: 'unsigned' ;
GEN     : 'gen' ;
TYPE    : 'type' ;
ASSERT  : 'assert' ;
ASSUME  : 'assume' ;

SYNC        : 'sync';
SYNC_READY  : 'sync' (WS|CMT)* 'ready';
SYNC_ACCEPT : 'sync' (WS|CMT)* 'accept';

WIRE        : 'wire';
BUBBLE      : 'bubble';
FSLICE      : 'fslice';
BSLICE      : 'bslice';

VERBATIM: 'verbatim' -> pushMode(VERBATIMENTRY_MODE);

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

mode VERBATIMENTRY_MODE;

VERBATIMENTRY_ENTITY: ENTITY -> type(ENTITY), popMode;

VERBATIMENTRY_WS: WS -> channel(WHITESPACE);

VERBATIMENTRY_CMT: CMT -> channel(COMMENT);

VERBATIMENTRY_IDENTIFIER: IDENTIFIER -> type(IDENTIFIER), Mode(VERBATIM_MODE);

VERBATIMENTRY_ERRORCHAR : . ;

////////////////////////////////////////////////////////////////////////////////

mode VERBATIM_MODE;

VERBATIM_WS: WS -> channel(WHITESPACE);

VERBATIM_CMT: CMT -> channel(COMMENT);

VERBATIM_BODY: '{' ( VERBATIM_BODY | ~[{}] )* '}'  -> popMode;

VERBATIM_ERRORCHAR : . ;
