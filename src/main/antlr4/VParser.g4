parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start
  : expr EOF
  ;
  
expr:
  expr BINARY_OP expr # BinaryExpr
  | sourceText       # SoleExpr
  ;

sourceText
  : IDENTIFIER
  ;
