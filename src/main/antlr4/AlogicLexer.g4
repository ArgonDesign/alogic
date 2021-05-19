////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Antlr4 lexer grammar for Alogic
////////////////////////////////////////////////////////////////////////////////

lexer grammar AlogicLexer;

channels {
	COMMENT
}

fragment LCMT: '//' ~[\n]*   ;           // Line comment
fragment BCMT: '/*'  .*? '*/';           // Block comment
CMT: (LCMT | BCMT) -> channel(COMMENT) ; // Any comment

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

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;

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
FSM         : 'fsm';
NETWORK     : 'network';
PIPELINE    : 'pipeline';
TYPEDEF     : 'typedef';
STRUCT      : 'struct';
IN          : 'in';
OUT         : 'out';
SNOOP       : 'snoop';
CONST       : 'const' ;
PARAM       : 'param' ;
FENCE       : 'fence' ;
TRUE        : 'true' ;
FALSE       : 'false' ;
VOID        : 'void' ;
BOOL        : 'bool';
UINT        : 'uint' ;
INT         : 'int' ;
LOOP        : 'loop' ;
WHILE       : 'while' ;
DO          : 'do' ;
FOR         : 'for' ;
IF          : 'if' ;
GOTO        : 'goto' ;
ELSE        : 'else' ;
BREAK       : 'break' ;
CONTINUE    : 'continue' ;
RETURN      : 'return' ;
CASE        : 'case' ;
DEFAULT     : 'default' ;
NEW         : 'new' ;
BIND        : 'bind' ;
LET         : 'let' ;
ENTITY      : 'entity' ;
STATIC      : 'static' ;
WAIT        : 'wait' ;
COMB        : 'comb'  ;
STACK       : 'stack' ;
SRAM        : 'sram' ;
REG         : 'reg' ;
SIGNED      : 'signed' ;
UNSIGNED    : 'unsigned' ;
GEN         : 'gen' ;
TYPE        : 'type' ;
ASSERT      : 'assert' ;
ASSUME      : 'assume' ;
UNREACHABLE : 'unreachable' ;
IMPORT      : 'import' ;
USING       : 'using' ;
FROM        : 'from' ;
THIS        : 'this' ;
AS          : 'as' ;
COMPILE     : 'compile' ;

SYNC        : 'sync';
SYNC_READY  : 'sync' (WS|NL|CMT)* 'ready';

WIRE        : 'wire';
BUBBLE      : 'bubble';
FSLICE      : 'fslice';
BSLICE      : 'bslice';

VERBATIM: 'verbatim' -> pushMode(VERBATIMENTRY_MODE);

IDENTIFIER: SIMPLEID;

WS: [ \t\r]+ -> channel(HIDDEN);

// This is not hidden here as knowing where newlines are is requried to
// implement the preprocessor directives below. All NL tokens will eventually
// be hidden by AlogicTokenFactory so the parser will not see one ever.
NL: '\n';

// Preprocessor directives. These are implemented by AlogicTokenFactory,
// which subsequently hides these tokens, so the parser will never see them.
HASHLINE : '#' WS* 'line';

// Match anything else. This rule ensures the lexer never causes a syntax
// error and therefore we can handle all errros in the parser for simplicity.
ERRORCHAR : . ;

////////////////////////////////////////////////////////////////////////////////

mode VERBATIMENTRY_MODE;

VERBATIMENTRY_ENTITY: ENTITY -> type(ENTITY), popMode;

VERBATIMENTRY_WS: WS -> type(WS), channel(HIDDEN);

VERBATIMENTRY_NL: NL -> type(NL), channel(HIDDEN);

VERBATIMENTRY_CMT: CMT -> type(CMT), channel(HIDDEN);

VERBATIMENTRY_IDENTIFIER: IDENTIFIER -> type(IDENTIFIER), Mode(VERBATIM_MODE);

VERBATIMENTRY_ERRORCHAR : . -> type(ERRORCHAR);

////////////////////////////////////////////////////////////////////////////////

mode VERBATIM_MODE;

VERBATIM_WS: WS -> type(WS), channel(HIDDEN);

VERBATIM_NL: NL -> type(NL), channel(HIDDEN);

VERBATIM_CMT: CMT -> type(CMT), channel(HIDDEN);

VERBATIM_BODY: '{' ( VERBATIM_BODY | ~[{}] )* '}'  -> popMode;

VERBATIM_ERRORCHAR : . -> type(ERRORCHAR);
