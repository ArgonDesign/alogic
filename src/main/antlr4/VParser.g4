parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start
  : sourceText EOF
  ;

sourceText
  : (idents+=IDENTIFIER)*
  ;
