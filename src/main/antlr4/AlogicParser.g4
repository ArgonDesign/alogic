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
}

////////////////////////////////////////////////////////////////////////////////
// Start rule for whole source file
////////////////////////////////////////////////////////////////////////////////

root : riz* EOF ;

////////////////////////////////////////////////////////////////////////////////
// Identifiers
////////////////////////////////////////////////////////////////////////////////

ident : IDENTIFIER ('#' '[' expr (',' expr)* ']')? ;

////////////////////////////////////////////////////////////////////////////////
// Descriptions (introducint a name)
////////////////////////////////////////////////////////////////////////////////

desc : attributes? descbase ;

attributes : '(*' attr (',' attr)* '*)' ;

attr
  : IDENTIFIER              # AttrFlag
  | IDENTIFIER '=' expr     # AttrExpr
  | IDENTIFIER '=' slices   # AttrSlices
  ;

descbase
  : expr ident ('=' init=expr)? ';'                                     # DescVar
  | 'in' fct? expr ident ';'                                            # DescIn
  | 'out' fct? stt? expr ident ('=' init=expr)? ';'                     # DescOut
  | 'pipeline' expr ident ';'                                           # DescPipeline
  | 'param' expr IDENTIFIER ('=' init=expr)?  ';'                       # DescParam
  | 'const' expr IDENTIFIER '=' expr  ';'                               # DescConst
  | expr ident '[' expr ']' ';'                                         # DescArr
  | 'sram' (wire='wire')? expr ident '[' expr ']' ';'                   # DescSram
  | 'typedef' expr ident ';'                                            # DescType
  | entity_keyword ident '{' ent* '}'                                   # DescEntity
  | 'struct' ident '{' rec* '}'                                         # DescRecord
  | ident '=' 'new' expr ';'                                            # DescInstance
  | 'new' entity_keyword ident '{' ent* '}'                             # DescSingleton
  | (stat='static')? expr ident '(' formal_arguments? ')' '{' stmt* '}' # DescFuncAlogic
  | 'import' expr IDENTIFIER '('formal_arguments? ')' ';'               # DescFuncImport
  ;

fct
  : 'sync'        # FCTSync
  | SYNC_READY    # FCTSyncReady
  | SYNC_ACCEPT   # FCTSyncAccept
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

////////////////////////////////////////////////////////////////////////////////
// Gen constructs
////////////////////////////////////////////////////////////////////////////////

gen : 'gen' generate ;

generate
  : 'if' '(' thenCond=expr ')' '{' thenItems=genitems '}'
    ('else' 'if' '(' elifCond+=expr ')' '{' elifItems+=genitems '}')*
    ('else' '{' elseItems=genitems '}')?                                    # GenIf
  | 'for' '(' ginits ';' expr ';' lsteps ')' '{' genitems '}'               # GenFor
  | 'for' '(' expr IDENTIFIER op=('<' | '<=')  expr ')' '{' genitems '}'    # GenRange
  ;

genitems : genitem* ;
genitem
  : gen         # GenItemGen
  | desc        # GenItemDesc
  | assertion   # GenItemAssertion
  | stmt        # GenItemStmt
  | kase        # GenItemCase
  | ent         # GenItemEnt
  | rec         # GenItemRec
  ;

ginits : ginit (',' ginit)* ;
ginit : expr IDENTIFIER point='=' expr ;

////////////////////////////////////////////////////////////////////////////////
// Assertions
////////////////////////////////////////////////////////////////////////////////

assertion
  : 'assert' expr (',' STRING)? ';'             # AssertionAssert
  | 'static' 'assert' expr (',' STRING)? ';'    # AssertionStatic
  ;

////////////////////////////////////////////////////////////////////////////////
// File contents
////////////////////////////////////////////////////////////////////////////////


riz
  : desc    # RizDesc
  ;

////////////////////////////////////////////////////////////////////////////////
// Entity contents
////////////////////////////////////////////////////////////////////////////////

ent
  : desc                                                # EntDesc
  | gen                                                 # EntGen
  | lhs=expr point='->' rhs+=expr (',' rhs+=expr)* ';'  # EntConnect
  | 'fence' '{' stmt* '}'                               # EntFenceBlock
  | assertion                                           # EntAssertion
  | 'verbatim' IDENTIFIER VERBATIM_BODY                 # EntVerbatimBlock
  ;

////////////////////////////////////////////////////////////////////////////////
// Record contents
////////////////////////////////////////////////////////////////////////////////

rec
  : desc        # RecDesc
  | gen         # RecGen
  | assertion   # RecAssertion
  ;

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

stmt
  : desc                                                        # StmtDesc
  | gen                                                         # StmtGen
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
  | assertion                                                   # StmtAssertion
  ;

kase
  : gen                         # CaseGen
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
  // 'this'
  | 'this'                                                      # ExprThis
  // Names
  | ident                                                       # ExprIdent
  | ATID                                                        # ExprAtid
  | DOLLARID                                                    # ExprDollarid
  // Call
  | expr open='(' args? ')'                                     # ExprCall
  // Index/Slice
  | expr '[' idx=expr ']'                                       # ExprIndex
  | expr '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']'     # ExprSlice
  // Select
  | expr '.' ident                                              # ExprSelect
  // Operators
  | op=('+' | '-' | '~' | '!' | '&' | '|' | '^' | '\'' ) expr   # ExprUnary
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
  |<assoc=right> expr op='?' expr ':' expr                      # ExprTernary
  | '{' expr s='{' expr (',' expr)* e='}' '}'                   # ExprRep
  | '{' expr (',' expr)* '}'                                    # ExprCat
  ;

args : arg (',' arg)* ;
arg
  : IDENTIFIER point='=' expr   # ArgNamed
  | expr                        # ArgPositional
  ;