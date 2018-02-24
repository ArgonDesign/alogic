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

// TODO: syntax changes:
//
// PROPOSED: Change
//   task ID '{' ports body '}'
// to:
//   task ID '('ports')' '{' body '}'
//
// PROPOSED: change
//   'void' 'verilog' '(')' '{'...'}'
// to:
//   'verilog' {...}
// or to something similar to:
//   'verbatim' 'verilog' '{'...'}'
//   (for when we would support multiple target languages???)

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
      (entity_decl ';')*
      (entity_content)*
    '}'
  ;

///////////////////////////////////////////////////////////////////////////////
// Declarations in entity scope
///////////////////////////////////////////////////////////////////////////////

entity_decl
  : 'out' flow_control_type? storage_type? kind IDENTIFIER  # EntityDeclOut
  | 'in' flow_control_type? kind IDENTIFIER                 # EntityDeclIn
  | 'param' kind IDENTIFIER '=' expr                        # EntityDeclParam
  | 'const' kind IDENTIFIER '=' expr                        # EntityDeclConst
  | 'pipeline' kind IDENTIFIER                              # EntityDeclPipeline
  | decl                                                    # EntityDeclTerm
  | 'verilog' decl                                          # EntityDeclVerilog
  ;

///////////////////////////////////////////////////////////////////////////////
// Entity contents
///////////////////////////////////////////////////////////////////////////////

entity_content
  : instance                            # EntityContentInstance
  | connect                             # EntityContentConnect
  | (autoinst='new')? entity            # EntityContentEntity
  | 'void' 'fence' '(' ')' block        # EntityContentFenceFunction
  | 'fence' block                       # EntityContentFenceBlock
  | 'void' IDENTIFIER '(' ')' block     # EntityContentFunction
  | VERILOGFUNC VERBATIMBODY            # EntityContentVerilogFuction
  | 'verbatim' IDENTIFIER VERBATIMBODY  # EntityContentVerbatimBlock
  ;

///////////////////////////////////////////////////////////////////////////////
// Content elements
///////////////////////////////////////////////////////////////////////////////

connect : lhs=connect_ref '->' rhs+=connect_ref (',' rhs+=connect_ref)* ';' ;

connect_ref
  : ref=connect_ref '.' IDENTIFIER # ConnectRefSelect
  | IDENTIFIER                     # ConnectRefIdent
  ;

instance : IDENTIFIER '=' 'new' IDENTIFIER '(' param_assigns ')' ';' ;

param_assigns : (IDENTIFIER '=' expr (','  IDENTIFIER '=' expr)*)? ;

///////////////////////////////////////////////////////////////////////////////
// Declarations
///////////////////////////////////////////////////////////////////////////////

decl
  : kind IDENTIFIER ('=' expr)?     # DeclVar
  | kind IDENTIFIER ('[' expr ']')+ # DeclArr
  ;

///////////////////////////////////////////////////////////////////////////////
// Port qualifiers
///////////////////////////////////////////////////////////////////////////////

flow_control_type
  : 'sync'        # FlowControlTypeSync
  | SYNC_READY    # FlowControlTypeSyncReady
  | SYNC_ACCEPT   # FlowControlTypeSyncAccept
  ;

storage_type
  : 'wire'                                  # StorageTypeWire
  | (slices+=('bubble' | 'freg' | 'breg'))+ # StorageTypeSlices
  ;

///////////////////////////////////////////////////////////////////////////////
// Statements
///////////////////////////////////////////////////////////////////////////////

block
  : '{' statement* '}'
  ;

statement
  : block                                                             # StmtBlock
  | 'if' '(' expr ')' thenStmt=statement ('else' elseStmt=statement)? # StmtIf
  | 'case' '(' expr ')' '{' case_clause+ '}'                          # StmtCase
  | let? 'loop' block                                                 # StmtLoop
  | let? 'do' block 'while' '(' expr ')' ';'                          # StmtDo
  | let? 'while' '(' expr ')' block                                   # StmtWhile
  | let? 'for' '(' loop_init  ';' expr ';' step=assignment ')' block  # StmtFor
  | 'goto' IDENTIFIER ';'                                             # StmtGoto
  | 'fence' ';'                                                       # StmtFence
  | 'break' ';'                                                       # StmtBreak
  | 'return' ';'                                                      # StmtReturn
  | decl ';'                                                          # StmtDecl
  | assignment ';'                                                    # StmtAssignment
  | expr ';'                                                          # StmtExpr
  | '$' '(' STRING ')' ';'                                            # StmtDollarComment
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

///////////////////////////////////////////////////////////////////////////////
// Expressions
///////////////////////////////////////////////////////////////////////////////

expr
  : '(' expr ')'                                                            # ExprBracket
  | expr '(' commaexpr ')'                                                  # ExprCall
  // Operators
  | op=('+' | '-' | '!' | '~' | '&' | '~&' | '|' | '~|' | '^' | '~^') expr  # ExprUnary
  | expr op=('*' | '/' | '%') expr                                          # ExprBinary
  | expr op=('+' | '-') expr                                                # ExprBinary
  | expr op=('<<' | '>>' | '>>>') expr                                      # ExprBinary // TODO: '<<<'
  | expr op=('>' | '>=' | '<' | '<=') expr                                  # ExprBinary
  | expr op=('==' | '!=') expr                                              # ExprBinary
  | expr op='&' expr                                                        # ExprBinary
  | expr op=('^' | '~^') expr                                               # ExprBinary
  | expr op='|' expr                                                        # ExprBinary
  | expr op='&&' expr                                                       # ExprBinary
  | expr op='||' expr                                                       # ExprBinary
  | expr '?' expr ':' expr                                                  # ExprTernary
  | '{' expr '{' expr '}' '}'                                               # ExprRep
  | '{' commaexpr '}'                                                       # ExprCat
  | expr '[' idx=expr ']'                                                   # ExprIndex
  | expr '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']'                 # ExprSlice
  // Select
  | expr '.' IDENTIFIER                                                     # ExprSelect
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
  ;

commaexpr : (expr (',' expr)*)? ;

