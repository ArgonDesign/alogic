////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

parser grammar VPreprocParser;

options {
  tokenVocab = VPreprocLexer;
}

start : entities EOF ;

entities : (entity)*;

entity
 : HASHINCLUDE LITERAL                        # HashInclude
 | HASHDEFINE VIDENTIFIER VREST               # HashDefine
 | IDENTIFIER                                 # Identifier
 | LITERAL                                    # Literal
 | ifcond=(HASHIF|HASHIFDEF) IDENTIFIER
   entities (HASHELSE entities)? HASHENDIF    # HashIf
 | ANYTHING                                   # Anything
 | LINE_COMMENT                               # LineComment
 | BLOCK_COMMENT                              # BlockComment
 ;
