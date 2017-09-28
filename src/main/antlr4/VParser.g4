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

parser grammar VParser;

options {
  tokenVocab = VLexer;
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
  : typedefinition*
    entity
    EOF
  ;

///////////////////////////////////////////////////////////////////////////////
// Type specifiers
///////////////////////////////////////////////////////////////////////////////

typedefinition
  : typedef
  | struct
  ;

typedef
  : 'typedef' known_type IDENTIFIER ';'
  ;

struct
  :  'struct' IDENTIFIER '{'
      (fields+=field)*
    '}' ';'
  ;

field: known_type IDENTIFIER SEMICOLON;

known_type
  : 'bool'                    # BoolType
  | INTTYPE                   # IntType
  | UINTTYPE                  # UintType
  | IDENTIFIER                # IdentifierType
  | 'int'  '(' commaexpr ')'  # IntVType
  | 'uint' '(' commaexpr ')'  # UintVType
  ;

///////////////////////////////////////////////////////////////////////////////
// Design entities
///////////////////////////////////////////////////////////////////////////////

entity
  : fsm_entity
  | verilog_entity
  | network_entity
  ;

fsm_entity
  : 'fsm' IDENTIFIER '{'
      (decls+=fsm_decl)*
      (contents+=fsm_content)*
    '}'                                 #EntityFSM
  ;

verilog_entity
  : 'verilog' IDENTIFIER '{'
      (decls+=entity_decl)*
      (contents+=verilog_function)*
    '}'                                 #EntityVerilog
  ;

network_entity
  : 'network' IDENTIFIER '{'
      (decls+=network_decl)*
      (contents+=network_content)*
    '}'                                 #EntityNetwork
  ;

///////////////////////////////////////////////////////////////////////////////
// Entity declarations
///////////////////////////////////////////////////////////////////////////////

entity_decl
  : 'out' flow_control_type? storage_type? port_type IDENTIFIER ';'   #TaskDeclOut
  | 'in' flow_control_type? port_type IDENTIFIER ';'                  #TaskDeclIn
  | 'param' known_type IDENTIFIER '=' expr ';'                        #TaskDeclParam
  | 'const' known_type IDENTIFIER '=' expr ';'                        #TaskDeclConst
  ;

fsm_decl
  : entity_decl                           #DUMMYRULENAME_FSM_DECL
  | 'verilog' decl ';'                    #TaskDeclVerilog
  | decl ';'                              #TaskDecl
  ;

network_decl
  : entity_decl                           #DUMMYRULENAME_NETWORK_DECL
  | 'pipeline' known_type IDENTIFIER ';'  #TaskDeclPipeline
  ;

///////////////////////////////////////////////////////////////////////////////
// Entity Contents
///////////////////////////////////////////////////////////////////////////////

fsm_content
  : function
  | fence_function
  | verilog_function
  ;

network_content
  : (autoinst='new')? fsm_entity  #NetworkContentFSM
  | connect                       #DUMMYRULENAME_NETWORK_CONTENT
  | instantiate                   #DUMMYRULENAME_NETWORK_CONTENT
  | verilog_function              #DUMMYRULENAME_NETWORK_CONTENT
  ;

///////////////////////////////////////////////////////////////////////////////
// Content elements
///////////////////////////////////////////////////////////////////////////////

connect : lhs=dotted_name '->' rhs+=dotted_name (',' rhs+=dotted_name)* ';' ;

instantiate : IDENTIFIER '=' 'new' IDENTIFIER '(' param_args ')' ';' ;

function
  : 'void' IDENTIFIER '(' ')' '{'
      (stmts += statement)*
    '}'
  ;

fence_function
  : 'void' 'fence' '(' ')' '{'
      (stmts += statement)*
    '}'                             # FenceFunction
  ;

verilog_function
  : VERILOGFUNC VERILOGBODY         # VerilogFunction
  ;

decl_var_noinit
  : known_type IDENTIFIER            #DeclVarNoInit
  ;

decl_var_init
  : known_type IDENTIFIER '=' expr   #DeclVarInit
  ;

decl_arr
  : known_type IDENTIFIER ('[' es+=expr ']')+   #DeclArr
  ;

decl
  : decl_var_noinit
  | decl_var_init
  | decl_arr
  ;

flow_control_type
  : 'sync'        #FlowControlTypeSync
  | SYNC_READY    #FlowControlTypeSyncReady
  | SYNC_ACCEPT   #FlowControlTypeSyncAccept
  ;

storage_type
  : 'wire'    #StorageTypeWire
  | 'bubble'  #StorageTypeBubble
  ;

port_type
  : known_type #PortTypeKnown
  | 'void'     #PortTypeVoid
  ;

///////////////////////////////////////////////////////////////////////////////
// Expressions
///////////////////////////////////////////////////////////////////////////////

unary_op : '+' | '-' | '!' | '~' | '&' | '~&' | '|' | '~|' | '^' | '~^' ;

// All expressions
expr
  : '(' expr ')'                                                # ExprBracket
  | ref=expr '(' commaexpr ')'                                  # ExprCall
  | ref=expr '[' idx=expr ']'                                   # ExprIndex
  | ref=expr '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']' # ExprSlice
  | ref=expr '.' IDENTIFIER                                     # ExprDot
  | IDENTIFIER                                                  # ExprId
  // Operators
  | unary_op expr                                               # ExprUnary
  | expr op=('*' | '/' | '%') expr                              # ExprMulDiv
  | expr op=('+' | '-') expr                                    # ExprAddSub
  | expr op=('<<' | '>>' | '>>>') expr                          # ExprShift   // TODO: '<<<'
  | expr op=('>' | '>=' | '<' | '<=') expr                      # ExprCompare
  | expr op=('==' | '!=') expr                                  # ExprEqual
  | expr op='&' expr                                            # ExprBAnd
  | expr op=('^' | '~^') expr                                   # ExprBXor
  | expr op='|' expr                                            # ExprBOr
  | expr op='&&' expr                                           # ExprAnd
  | expr op='||' expr                                           # ExprOr
  | expr '?' expr ':' expr                                      # ExprTernary
  | '{' expr '{' expr '}' '}'                                   # ExprRep
  | '{' commaexpr '}'                                           # ExprCat
  // Builtins
  | DOLLARID '(' commaexpr ')'                                  # ExprDollar
  | '@bits' '(' known_type ')'                                  # ExprAtBits
  | ATID '(' commaexpr ')'                                      # ExprAt
  // Literals
  | 'true'                                                      # ExprTrue
  | 'false'                                                     # ExprFalse
  | TICKNUM                                                     # ExprTrickNum
  | CONSTANT TICKNUM                                            # ExprConstTickNum
  | CONSTANT                                                    # ExprConst
  | LITERAL                                                     # ExprLiteral
  ;

///////////////////////////////////////////////////////////////////////////////
// Left value expressions
///////////////////////////////////////////////////////////////////////////////

lval
  : ref=lval '[' idx=expr ']'                                   # LValIndex
  | ref=lval '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']' # LValSlice
  | ref=lval '.' IDENTIFIER                                     # LValDot
  | IDENTIFIER                                                  # LValId
  | '{' lval (',' lval)+ '}'                                    # LValCat
  ;

///////////////////////////////////////////////////////////////////////////////
// Misc bits
///////////////////////////////////////////////////////////////////////////////

commaexpr : (expr)? (',' expr)* ;

param_args : (param_assign (',' param_assign)*)? ;

param_assign : IDENTIFIER '=' expr ;

dotted_name : (es+=IDENTIFIER) ('.' es+=IDENTIFIER)*;

///////////////////////////////////////////////////////////////////////////////
// Statements
///////////////////////////////////////////////////////////////////////////////

statement
  : '{' (stmts+=statement)* '}'                 # BlockStmt
  | decl ';'                                    # DeclStmt
  | 'loop' '{'
      (stmts += statement)*
    '}'                                         # LoopStmt
  | 'while' '(' expr ')' '{'
      (stmts += statement)*
    '}'                                         # WhileStmt
  | 'if' '(' expr ')'
      thenStmt=statement
    ('else'
      elseStmt=statement)?                      # IfStmt
  | 'case' '(' expr ')' '{'
      (cases+=case_stmt)+
    '}'                                         # CaseStmt
  | 'for' '(' init=for_init ';'
              cond=expr ';'
              step=assignment_statement ')' '{'
      (stmts += statement)*
    '}'                                         # ForStmt
  | 'do' '{'
      (stmts += statement)*
    '}' 'while' '(' expr ')' ';'                # DoStmt
  | 'fence' ';'                                 # FenceStmt
  | 'break' ';'                                 # BreakStmt
  | 'return' ';'                                # ReturnStmt
  | '$' '(' LITERAL ')' ';'                     # DollarCommentStmt
  | 'goto' IDENTIFIER ';'                       # GotoStmt
  | assignment_statement ';'                    # AssignmentStmt
  | expr ';'                                    # ExprStmt
  ;

case_stmt
  : 'default' ':' statement # DefaultCase
  | commaexpr ':' statement # NormalCase
  ;

for_init
  : assignment_statement  #ForInitNoDecl
  | decl_var_init         #ForInitDecl
  ;

assignment_statement
  : lval '++'           # AssignInc
  | lval '--'           # AssignDec
  | lval '=' expr       # Assign
  | lval ASSIGNOP expr  # AssignUpdate
  ;
