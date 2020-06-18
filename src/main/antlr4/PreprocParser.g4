////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Antlr4 parser grammar for preprocessor
////////////////////////////////////////////////////////////////////////////////

parser grammar PreprocParser;

options {
  tokenVocab = PreprocLexer;
  contextSuperClass = com.argondesign.alogic.antlr.AlogicParserRuleContext;
}

start : entities EOF ;

entities : (entity)*;

entity
 : HASHINCLUDE LITERAL                        # HashInclude
 | HASHDEFINE IDENTIFIER REST                 # HashDefine
 | IDENTIFIER                                 # Identifier
 | LITERAL                                    # Literal
 | ifcond=(HASHIF|HASHIFDEF) IDENTIFIER
   entities (HASHELSE entities)? HASHENDIF    # HashIf
 | ANYTHING                                   # Anything
 | LINE_COMMENT                               # LineComment
 | BLOCK_COMMENT                              # BlockComment
 ;
