lexer grammar VLexer;

channels {
  WHITESPACE,
  COMMENT
}

UINTTYPE: 'u' [0-9]+;

INTTYPE: 'i' [0-9]+;

TICKNUM: '\'' 's'? [bdhx] [0-9a-fA-F_]+ ;

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
NAND:       '~&'  ;
OR:         '|'   ;
NOR:        '~|'  ;
XOR:        '^'   ;
XNOR:       '~^'  ;

LSHIFT:     '<<'  ;
URSHIFT:    '>>'  ;
SRSHIFT:    '>>>' ;

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
  : '+='
  | '-='
  | '&='
  | '|='
  | '^='
  | '>>='
  | '<<='
  | '>>>='
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

SYNC_READY_BUBBLE:  'sync' WS 'ready' WS 'bubble';
WIRE_SYNC_ACCEPT:  'wire' WS 'sync' WS 'accept';
SYNC_READY: 'sync' WS 'ready';
WIRE_SYNC: 'wire' WS 'sync';
SYNC_ACCEPT: 'sync' WS 'accept';
SYNC: 'sync';
WIRE: 'wire';

LITERAL: '"' ~["]* '"';

VERILOGFUNC: 'void' WS? 'verilog' WS? '(' WS? ')' WS? -> pushMode(VMODE);

CONSTANT: [0-9_]+;

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

mode VMODE;

VERILOGBODY:  '{' ( VERILOGBODY | ~[{}] )* '}'  -> popMode;

