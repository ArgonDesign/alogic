lexer grammar VLexer;

channels {
  WHITESPACE,
  COMMENT
}

UINTTYPE: 'u' [0-9]+;

INTTYPE: 'i' [0-9]+;
  
BOOL: 'bool';
  
TICKNUM: '\'' 's'? [bdhx] [0-9a-fA-F_]+ ;

// TODO Add verilog_body

COLON: ':';

DOLLAR: '$' SIMPLEID;

DOLLARCOMMENT: '$';

GOESTO: '->';

EQUALS: '=';
ASSIGNOP: '+=' | '-=' | '&=' | '|=' | '^=' | '>>=' | '<<=' | '>>>=' ;

BINARYOP: '+' | '&&' | '||' | '<' | '>' | '<<' | '>>' | '^' | '>>>' | '!=' | '==' | '*' | '<=' | '>=';

AND: '&';
OR: '|';
TILDA: '~';
NOT: '!';
COMMA: ',';
SEMICOLON: ';';

PLUSPLUS: '++';
MINUSMINUS: '--';

DOT: '.';
LEFTCURLY: '{';
RIGHTCURLY: '}';
LEFTSQUARE: '[';
RIGHTSQUARE: ']';
LEFTBRACKET: '(';
RIGHTBRACKET: ')';

MINUSCOLON: '-:';
PLUSCOLON: '+:';

QUESTIONMARK: '?';

FSM: 'fsm';
NETWORK: 'network';
PIPELINE: 'pipeline';
TYPEDEF: 'typedef';
STRUCT: 'struct';
IN: 'in';
OUT: 'out';
PARAM: 'const' | 'param';  // TODO move 'const' option and use it instead of #defines
FENCE   : 'fence' ;   
TRUE    : 'true' ;    
FALSE   : 'false' ;   
VOID    : 'void' ;    
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
 
SYNC_READY_BUBBLE:  'sync' WS 'ready' WS 'bubble';
WIRE_SYNC_ACCEPT:  'wire' WS 'sync' WS 'accept';
SYNC_READY: 'sync' WS 'ready';
WIRE_SYNC: 'wire' WS 'sync';
SYNC_ACCEPT: 'sync' WS 'accept';
SYNC: 'sync';
WIRE: 'wire';

LITERAL: '"' ~["]* '"';

HASHDEFINE: '#' [ \t]* ('define' | 'def');

VERILOGBODY: 'void' WS? 'verilog' WS? '(' WS? ')' WS? '{' -> pushMode(VMODE);

CONSTANT: [0-9_]+;

IDENTIFIER: SIMPLEID;


ONE_LINE_COMMENT
  : '//' .*? NL -> channel(COMMENT)
  ;

BLOCK_COMMENT
  : '/*'  .*? '*/' -> channel(COMMENT)
  ;

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;

MINUS: '-';
  
BINARY_OP: '+' | '*';

fragment NL
  : '\r'? '\n'
  ;

WS
  : ([ \t] | NL)+ -> channel(WHITESPACE)
  ;

ERRORCHAR : . ;

mode VMODE;

VLEFTCURLY: '{' -> pushMode(VMODE);

VRIGHTCURLY: '}' -> popMode;

VANY : . ;



