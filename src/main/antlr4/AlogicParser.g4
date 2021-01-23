////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Antlr4 parser grammar for Alogic
////////////////////////////////////////////////////////////////////////////////

parser grammar AlogicParser;

options {
  tokenVocab = AlogicLexer;
  contextSuperClass = com.argondesign.alogic.antlr.AlogicParserRuleContext;
}

////////////////////////////////////////////////////////////////////////////////
// Start rule for whole source file (aka package, but 'package' is a keyword)
////////////////////////////////////////////////////////////////////////////////

file : pkg* EOF ;

////////////////////////////////////////////////////////////////////////////////
// Identifiers
////////////////////////////////////////////////////////////////////////////////

ident : IDENTIFIER ('#' '[' expr (',' expr)* ']')? ;

////////////////////////////////////////////////////////////////////////////////
// Descriptions (introducing a name)
////////////////////////////////////////////////////////////////////////////////

desc : attributes? descbase ;

attributes : '(*' attr (',' attr)* '*)' ;

attr
  : IDENTIFIER              # AttrBool
  | IDENTIFIER '=' expr     # AttrExpr
  ;

descbase
  : (sttc='static')? expr ident ('=' init=expr)? ';'                            # DescVar
  | in='in' fct? (spec=expr | 'pipeline') ident? ';'                            # DescIn
  | out='out' fct? stt? (spec=expr | 'pipeline') ident? ('=' init=expr)? ';'    # DescOut
  | 'pipeline' expr ident ';'                                                   # DescPipeVar
  | 'param' expr ident ('=' init=expr)?  ';'                                    # DescParam
  | 'param' 'type' ident ('=' init=expr)?  ';'                                  # DescParamType
  | 'const' expr ident '=' expr  ';'                                            # DescConst
  | expr ident '[' expr ']' ';'                                                 # DescArr
  | 'sram' (wire='wire')? expr ident '[' expr ']' ';'                           # DescSram
  | 'typedef' expr ident ';'                                                    # DescType
  | entity_keyword ident '{' ent* '}'                                           # DescEntity
  | 'struct' ident '{' rec* '}'                                                 # DescRecord
  | ident '=' 'new' expr ';'                                                    # DescInstance
  | 'new' entity_keyword ident '{' ent* '}'                                     # DescSingleton
  | (stat='static')? expr ident '(' formal_arguments? cpar=')' '{' stmt* '}'    # DescFuncAlogic
  | 'import' expr IDENTIFIER '('formal_arguments? ')' ';'                       # DescFuncImport
  | 'gen' 'if' '(' conds+=expr cpar=')' (':' ident)? '{' thenItemss+=genitems '}'
    ('else' 'if' '(' conds+=expr ')' '{' thenItemss+=genitems '}')*
    ('else' '{' elseItems=genitems '}')?                                        # DescGenIf
  | 'gen' 'for' '(' ginits ';' expr ';' lsteps cpar=')' (':' ident)?
    '{' genitems '}'                                                            # DescGenFor
  | 'gen' 'for' '(' expr IDENTIFIER op=('<' | '<=')  expr cpar=')' (':' ident)?
    '{' genitems '}'                                                            # DescGenRange
  ;

fct
  : 'sync'        # FCTSync
  | SYNC_READY    # FCTSyncReady
  ;

slices : (slice+=('bubble' | 'fslice' | 'bslice'))+ ;

stt
  : 'wire' # STTWire
  | slices # STTSlices
  ;

entity_keyword
  : 'fsm'
  | 'network'
  | 'verbatim' 'entity'
  ;

formal_arguments
  : expr IDENTIFIER (',' expr IDENTIFIER)*
  ;

genitems : genitem* ;
genitem
  : desc        # GenItemDesc
  | imprt       # GenItemImport
  | usng        # GenItemUsing
  | from        # GenItemFrom
  | assertion   # GenItemAssertion
  | pkg         # GenItemPkg
  | ent         # GenItemEnt
  | rec         # GenItemRec
  | stmt        # GenItemStmt
  | kase        # GenItemCase
  ;

ginits : ginit (',' ginit)* ;
ginit : expr IDENTIFIER point='=' expr ;

////////////////////////////////////////////////////////////////////////////////
// Imports
////////////////////////////////////////////////////////////////////////////////

imprt
  : 'import' STRING 'as' ident ';' # ImportOne
  ;

////////////////////////////////////////////////////////////////////////////////
// Using
////////////////////////////////////////////////////////////////////////////////

usng
  : 'using' expr ('as' ident)? ';'  # UsingOne
  | 'using' expr '.' '*' ';'        # UsingAll
  ;

////////////////////////////////////////////////////////////////////////////////
// From (sugar for import + using)
////////////////////////////////////////////////////////////////////////////////

from
  : 'from' STRING 'import' expr ('as' ident)? ';'   # FromOne
  | 'from' STRING 'import' '*' ';'                  # FromAll
  ;

////////////////////////////////////////////////////////////////////////////////
// Assertions
////////////////////////////////////////////////////////////////////////////////

assertion
  : 'assert' expr (',' STRING)? ';'             # AssertionAssert
  | 'static' 'assert' expr (',' STRING)? ';'    # AssertionStatic
  ;

////////////////////////////////////////////////////////////////////////////////
// Package (file) contents
////////////////////////////////////////////////////////////////////////////////

pkg
  : desc                                # PkgDesc
  | imprt                               # PkgImport
  | usng                                # PkgUsing
  | from                                # PkgFrom
  | assertion                           # PkgAssertion
  | 'compile' expr ('as' ident)? ';'    # PkgCompile
  ;

////////////////////////////////////////////////////////////////////////////////
// Entity contents
////////////////////////////////////////////////////////////////////////////////

ent
  : desc                                                # EntDesc
  | imprt                                               # EntImport
  | usng                                                # EntUsing
  | from                                                # EntFrom
  | assertion                                           # EntAssertion
  | lhs=expr point='->' rhs+=expr (',' rhs+=expr)* ';'  # EntConnect
  | 'fence' '{' stmt* '}'                               # EntFenceBlock
  | 'verbatim' IDENTIFIER VERBATIM_BODY                 # EntVerbatimBlock
  ;

////////////////////////////////////////////////////////////////////////////////
// Record contents
////////////////////////////////////////////////////////////////////////////////

rec
  : desc        # RecDesc
  | imprt       # RecImport
  | usng        # RecUsing
  | from        # RecFrom
  | assertion   # RecAssertion
  ;

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

stmt
  : desc                                                        # StmtDesc
  | imprt                                                       # StmtImport
  | usng                                                        # StmtUsing
  | from                                                        # StmtFrom
  | assertion                                                   # StmtAssertion
  | '{' stmt* '}'                                               # StmtBlock
  | 'if' '(' expr ')' thenStmt=stmt ('else' elseStmt=stmt)?     # StmtIf
  | 'case' '(' expr ')' '{' kase* '}'                           # StmtCase
  | 'loop' '{' stmt* '}'                                        # StmtLoop
  | 'do' '{' stmt* '}' 'while' '(' expr ')' ';'                 # StmtDo
  | 'while' '(' expr ')' '{' stmt* '}'                          # StmtWhile
  | 'for' '(' linits?  ';' expr? ';' lsteps? ')' '{' stmt* '}'  # StmtFor
  | 'let' '(' linits ')' stmt                                   # StmtLet
  | 'fence' ';'                                                 # StmtFence
  | 'break' ';'                                                 # StmtBreak
  | 'continue' ';'                                              # StmtContinue
  | 'goto' expr ';'                                             # StmtGoto
  | 'return' expr? ';'                                          # StmtReturn
  | expr point='=' expr ';'                                     # StmtAssign
  | expr ASSIGNOP expr ';'                                      # StmtUpdate
  | expr op=('++'|'--') ';'                                     # StmtPost
  | expr ';'                                                    # StmtExpr
  | 'wait' expr? ';'                                            # StmtWait
  ;

kase
  : desc                        # CaseDesc // This is here to supprot 'gen' only
  | expr (',' expr)* ':' stmt   # CaseRegular
  | 'default' ':' stmt          # CaseDefault
  ;

linits : linit (',' linit)* ;
linit
  : expr point='=' expr             # LoopInitAssign
  | expr IDENTIFIER point='=' expr  # LoopInitDesc
  ;

lsteps : lstep (',' lstep)* ;
lstep
  : expr point='=' expr # LoopStepAssign
  | expr ASSIGNOP expr  # LoopStepUpdate
  | expr op=('++'|'--') # LoopStepPost
  ;

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

expr
  : '(' expr ')'                                                # ExprBracket
  // Literals
  | 'true'                                                      # ExprLitTrue
  | 'false'                                                     # ExprLitFalse
  | sign=('+' | '-')? SIZEDINT                                  # ExprLitSizedInt
  | sign=('+' | '-')? UNSIZEDINT                                # ExprLitUnsizedInt
  | STRING                                                      # ExprLitString
  // Primitive types
  | 'bool'                                                      # ExprTypeBool
  | INTTYPE                                                     # ExprTypeSInt
  | UINTTYPE                                                    # ExprTypeUInt
  | 'int'                                                       # ExprTypeSNum
  | 'uint'                                                      # ExprTypeUNum
  | 'void'                                                      # ExprTypeVoid
  // Keywords
  | ('in' | 'out')                                              # ExprKeyword
  | 'this'                                                      # ExprThis
  // Names
  | ident                                                       # ExprIdent
  | ATID                                                        # ExprAtid
  | DOLLARID                                                    # ExprDollarid
  // Call
  | expr point='(' args? ')'                                    # ExprCall
  // Index/Slice
  | expr point='[' idx=expr ']'                                     # ExprIndex
  | expr point='[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']'   # ExprSlice
  // Select
  | expr point='.' (ident | inout=('in' | 'out'))               # ExprDot
  // Operators
  | op=('+' | '-' | '~' | '!' | '&' | '|' | '^' | '\'' ) expr   # ExprUnary
  | expr op='\'' expr                                           # ExprBinary
  | expr op=('*' | '/' | '%') expr                              # ExprBinary
  | expr op=('+' | '-') expr                                    # ExprBinary
  | expr op=('<<' | '>>' | '>>>' | '<<<' ) expr                 # ExprBinary
  | expr op=('>' | '>=' | '<' | '<=') expr                      # ExprBinary
  | expr op=('==' | '!=') expr                                  # ExprBinary
  | expr op='&' expr                                            # ExprBinary
  | expr op='^' expr                                            # ExprBinary
  | expr op='|' expr                                            # ExprBinary
  | expr op='&&' expr                                           # ExprBinary
  | expr op='||' expr                                           # ExprBinary
  |<assoc=right> expr point='?' expr ':' expr                   # ExprTernary
  | '{' expr s='{' expr (',' expr)* e='}' '}'                   # ExprRep
  | '{' expr (',' expr)* '}'                                    # ExprCat
  ;

args : arg (',' arg)* ;
arg
  : ident point='=' expr    # ArgNamed
  | expr                    # ArgPositional
  ;