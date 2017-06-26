parser grammar VPreprocParser;

options {
  tokenVocab = VPreprocLexer;
}

start : entities EOF ;

entities : (entity)*;

entity
 : HASHINCLUDE LITERAL # HashInclude
 | HASHDEFINE VIDENTIFIER VREST # HashDefine
 | IDENTIFIER # Identifier
 | LITERAL # Literal
 | HASHIF IDENTIFIER entities (HASHELSE entities)? HASHENDIF # HashIf
 | ANYTHING # Anything
 | ONE_LINE_COMMENT # OneLineComment
 | BLOCK_COMMENT # BlockComment
 ;
