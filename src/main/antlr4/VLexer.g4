lexer grammar VLexer;

channels {
  WHITESPACE,
  COMMENT
}

UINTTYPE: 'u' [0-9]+;

INTTYPE: 'i' [0-9]+;
  
BOOL: 'bool';
  
TICKNUM: '\'' [bdhx] [0-9a-fA-F_]+ ;

NUMBER: [0-9_]+;

// TODO Add verilog_body

COLON: ':';

DOLLAR: '$' SIMPLEID;

ASSIGNOP: '=' | '+=' | '-=' | '&=' | '|=' | '^=' | '>>=' | '<<=' | '>>>=' ;

BINARYOP: '+' | '-' | '&&' | '||' | '<' | '>' | '<<' | '>>' | '^' | '>>>' | '!=' | '==' | '*' | '<=' | '>=';

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
CONST: 'const';
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

LITERAL: '"' ~["]* '"';

IDENTIFIER: SIMPLEID;

ONE_LINE_COMMENT
  : '//' .*? NL -> channel(COMMENT)
  ;

BLOCK_COMMENT
  : '/*'  .*? '*/' -> channel(COMMENT)
  ;

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;
  
BINARY_OP: '+' | '-' | '*';

fragment NL
  : '\r'? '\n'
  ;

WS
  : ([ \t] | NL)+ -> channel(WHITESPACE)
  ;

ERRORCHAR : . ;
