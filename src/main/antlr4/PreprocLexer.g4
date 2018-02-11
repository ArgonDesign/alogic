////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-208 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module : Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Antlr4 lexer grammar for preprocessor
////////////////////////////////////////////////////////////////////////////////

lexer grammar PreprocLexer;

// This grammar handles #define/#if/#ifdef/#else/#endif
// Comments are not stripped in order to keep the line numbers accurate for error messages in later stages
//

HASHINCLUDE: '#' [ \t]* 'include' [ \t]+;

HASHDEFINE: '#' [ \t]* ('define' | 'def') [ \t]+ -> pushMode(DEFINEMODE);

HASHIF: '#' [ \t]* 'if' [ \t]+;

HASHIFDEF: '#' [ \t]* 'ifdef' [ \t]+;

HASHELSE: '#' [ \t]* 'else';

HASHENDIF: '#' [ \t]* 'endif';

IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_$]*;

LITERAL: '"' ~["]* '"';

LINE_COMMENT: '//' .*? NL;

BLOCK_COMMENT: '/*'  .*? '*/';

fragment NL: '\r'? '\n';

ANYTHING : . ;

mode DEFINEMODE;

DEFINEIDENTIFIER: IDENTIFIER -> type(IDENTIFIER), Mode(DEFINERESTMODE);

mode DEFINERESTMODE;

// TODO: should support line continuation with \
REST: ~[\r\n/*()]+ -> popMode;

