
////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
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

///////////////////////////////////////////////////////////////////////////////
// Start rule for whole source file
///////////////////////////////////////////////////////////////////////////////

start
  : (type_definition ';')*
    entity
    EOF
  ;

///////////////////////////////////////////////////////////////////////////////
// Type definitions
///////////////////////////////////////////////////////////////////////////////

type_definition
  : 'typedef' kind IDENTIFIER   #TypeDefinitionTypedef
  | 'struct' IDENTIFIER '{'
      field+
    '}'                         #TypeDefinitionStruct
  ;

field: kind IDENTIFIER SEMICOLON;

///////////////////////////////////////////////////////////////////////////////
// Type names
///////////////////////////////////////////////////////////////////////////////

kind
  : 'bool'                    # TypeBool
  | INTTYPE                   # TypeInt
  | UINTTYPE                  # TypeUInt
  | 'int'  '(' commaexpr ')'  # TypeIntV
  | 'uint' '(' commaexpr ')'  # TypeUIntV
  | IDENTIFIER                # TypeIdent
  | 'void'                    # TypeVoid
  ;

///////////////////////////////////////////////////////////////////////////////
// Entity
///////////////////////////////////////////////////////////////////////////////

entity
  : variant=('fsm' | 'network' | 'verilog') IDENTIFIER '{'
      (decl ';')*
      (entity_content)*
    '}'
  ;

///////////////////////////////////////////////////////////////////////////////
// Declarations
///////////////////////////////////////////////////////////////////////////////

decl
  : kind IDENTIFIER ('=' expr)?                             # DeclVar
  | kind IDENTIFIER ('[' expr ']')+                         # DeclArr
  | 'out' flow_control_type? storage_type? kind IDENTIFIER  # DeclOut
  | 'in' flow_control_type? kind IDENTIFIER                 # DeclIn
  | 'param' kind IDENTIFIER '=' expr                        # DeclParam
  | 'const' kind IDENTIFIER '=' expr                        # DeclConst
  | 'pipeline' kind IDENTIFIER                              # DeclPipeline
  ;

flow_control_type
  : 'sync'        # FlowControlTypeSync
  | SYNC_READY    # FlowControlTypeSyncReady
  | SYNC_ACCEPT   # FlowControlTypeSyncAccept
  ;

storage_type
  : 'wire'                                      # StorageTypeWire
  | (slices+=('bubble' | 'fslice' | 'bslice'))+ # StorageTypeSlices
  ;

///////////////////////////////////////////////////////////////////////////////
// Entity contents
///////////////////////////////////////////////////////////////////////////////

entity_content
  : instance ';'                        # EntityContentInstance
  | connect ';'                         # EntityContentConnect
  | (autoinst='new')? entity            # EntityContentEntity
  | 'void' 'fence' '(' ')' block        # EntityContentFenceFunction
  | 'fence' block                       # EntityContentFenceBlock
  | 'void' IDENTIFIER '(' ')' block     # EntityContentFunction
  | VERILOGFUNC VERBATIMBODY            # EntityContentVerilogFuction
  | 'verbatim' IDENTIFIER VERBATIMBODY  # EntityContentVerbatimBlock
  ;

connect : lhs=expr '->' rhs+=expr (',' rhs+=expr)* ;

instance : IDENTIFIER '=' 'new' IDENTIFIER '(' param_assigns ')' ;

param_assigns : (IDENTIFIER '=' expr (','  IDENTIFIER '=' expr)*)? ;

///////////////////////////////////////////////////////////////////////////////
// Statements
///////////////////////////////////////////////////////////////////////////////

block
  : '{' statement* '}'
  ;

statement
  : block                                                               # StmtBlock
  | 'if' '(' expr ')' thenStmt=statement ('else' elseStmt=statement)?   # StmtIf
  | 'case' '(' expr ')' '{' case_clause+ '}'                            # StmtCase
  | let loop                                                            # StmtLet
  | loop                                                                # StatementLoop
  | 'goto' IDENTIFIER ';'                                               # StmtGoto
  | 'fence' ';'                                                         # StmtFence
  | 'break' ';'                                                         # StmtBreak
  | 'return' ';'                                                        # StmtReturn
  | decl ';'                                                            # StmtDecl
  | assignment ';'                                                      # StatementAssignment
  | expr ';'                                                            # StmtExpr
  | '$' '(' STRING ')' ';'                                              # StmtDollarComment
  ;

loop
  : 'loop' block                                                   # StmtLoop
  | 'do' block 'while' '(' expr ')' ';'                            # StmtDo
  | 'while' '(' expr ')' block                                     # StmtWhile
  | 'for' '(' loop_init?  ';' expr? ';' for_steps? ')' block       # StmtFor
  ;

let
  : 'let' '(' loop_init ')'
  ;

case_clause
  : 'default' ':' statement # DefaultCase
  | commaexpr ':' statement # NormalCase
  ;

assignment
  : expr '=' expr       # StmtAssign
  | expr ASSIGNOP expr  # StmtUpdate
  | expr op=('++'|'--') # StmtPost
  ;

loop_init
  : loop_init_item (',' loop_init_item)*
  ;

loop_init_item
  : expr '=' expr             # LoopInitAssign
  | kind IDENTIFIER '=' expr  # LoopInitDecl
  ;

for_steps
  : step+=assignment (',' step+=assignment)*
  ;

///////////////////////////////////////////////////////////////////////////////
// Expressions
///////////////////////////////////////////////////////////////////////////////

expr
  : '(' expr ')'                                                            # ExprBracket
  | expr '(' commaexpr ')'                                                  # ExprCall
  // Index/Slice
  | expr '[' idx=expr ']'                                                   # ExprIndex
  | expr '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']'                 # ExprSlice
  // Select
  | expr '.' IDENTIFIER                                                     # ExprSelect
  // Operators
  | op=('+' | '-' | '~' | '!' | '&' | '|' | '^' ) expr  # ExprUnary
  | expr op=('*' | '/' | '%') expr                                          # ExprBinary
  | expr op=('+' | '-') expr                                                # ExprBinary
  | expr op=('<<' | '>>' | '>>>' | '<<<' ) expr                             # ExprBinary
  | expr op=('>' | '>=' | '<' | '<=') expr                                  # ExprBinary
  | expr op=('==' | '!=') expr                                              # ExprBinary
  | expr op='&' expr                                                        # ExprBinary
  | expr op='^' expr                                                        # ExprBinary
  | expr op='|' expr                                                        # ExprBinary
  | expr op='&&' expr                                                       # ExprBinary
  | expr op='||' expr                                                       # ExprBinary
  | expr '?' expr ':' expr                                                  # ExprTernary
  | '{' expr '{' expr '}' '}'                                               # ExprRep
  | '{' commaexpr '}'                                                       # ExprCat
  // Builtins
  | ATID '(' commaexpr ')'                                                  # ExprAtCall
  | DOLLARID '(' commaexpr ')'                                              # ExprDollarCall
  // Literals
  | 'true'                                                                  # ExprTrue
  | 'false'                                                                 # ExprFalse
  | TICKNUM                                                                 # ExprTickNum
  | CONSTANT TICKNUM                                                        # ExprConstTickNum
  | CONSTANT                                                                # ExprConst
  | STRING                                                                  # ExprString
  // Name
  | IDENTIFIER                                                              # ExprIdent
  // Type
  | kind                                                                    # ExprType
  ;

commaexpr : (expr (',' expr)*)? ;

