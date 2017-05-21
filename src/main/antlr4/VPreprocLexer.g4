lexer grammar VPreprocLexer;

// This grammar handles #define/#if/#else/#endif
// Comments are not stripped in order to keep the line numbers accurate for error messages in later stages
//

HASHDEFINE: '#' [ \t]* ('define' | 'def') [ \t]+ -> pushMode(VMODE);

HASHIF: '#' [ \t]* 'if' [ \t]+;

HASHELSE: '#' [ \t]* 'else';

HASHENDIF: '#' [ \t]* 'endif';

IDENTIFIER: SIMPLEID;

ONE_LINE_COMMENT: '//' .*? NL;

BLOCK_COMMENT: '/*'  .*? '*/';

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;

fragment NL: '\r'? '\n';

ANYTHING : . ;

mode VMODE;

VIDENTIFIER: SIMPLEID -> Mode(DMODE);

mode DMODE;

VREST: ~[\r\n(//)(/*)]+ -> popMode;

