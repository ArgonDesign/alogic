parser grammar VPreprocParser;

options {
  tokenVocab = VPreprocLexer;
}

start : entities EOF ;

entities : (entity)*;

entity : 
   HASHDEFINE VIDENTIFIER VREST # HashDefine
 | IDENTIFIER # Identifier
 | HASHIF IDENTIFIER entities (HASHELSE entities HASHENDIF? HASHENDIF) # HashIf
 | ANYTHING # Anything
 | ONE_LINE_COMMENT # OneLineComment
 | BLOCK_COMMENT # BlockComment
 ;
