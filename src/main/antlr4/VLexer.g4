lexer grammar VLexer;

channels {
  WHITESPACE,
  COMMENT,
  DIRECTIVE
}

ONE_LINE_COMMENT
  : '//' .*? NL -> channel(COMMENT)
  ;

BLOCK_COMMENT
  : '/*'  .*? '*/' -> channel(COMMENT)
  ;

fragment SIMPLEID
  : [a-zA-Z_][a-zA-Z0-9_$]*
  ;

IDENTIFIER
  : SIMPLEID
  ;

fragment NL
  : '\r'? '\n'
  ;

WS
  : ([ \t] | NL)+ -> channel(WHITESPACE)
  ;

ERRORCHAR : . ;