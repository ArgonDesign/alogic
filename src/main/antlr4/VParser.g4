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

start
  : typedefinition*
    entity
    EOF
  ;

typedefinition
  : typedef
  | struct
  ;

entity
  : task
  | network
  ;

typedef
  : 'typedef' known_type IDENTIFIER ';'
  ;

struct
  :  'struct' IDENTIFIER '{'
      (fields+=field)*
    '}' ';'
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

network_decl
  : 'out' flow_control_type? storage_type? port_type IDENTIFIER ';'   #TaskDeclOut
  | 'in' flow_control_type? port_type IDENTIFIER ';'                  #TaskDeclIn
  | 'param' known_type IDENTIFIER '=' expr ';'                        #TaskDeclParam
  | 'const' known_type IDENTIFIER '=' expr ';'                        #TaskDeclConst
  | 'pipeline' known_type IDENTIFIER ';'                              #TaskDeclPipeline
  ;

task_decl
  : network_decl                        #DUMMYRULENAME
  | 'verilog' decl ';'                  #TaskDeclVerilog
  | decl ';'                            #TaskDecl
  ;

task
  : (autoinst='new')? 'fsm' IDENTIFIER '{'
      (decls+=task_decl)*
      (contents+=task_content)*
    '}'                                 #TaskFSM
  | 'verilog' IDENTIFIER '{'
      (decls+=task_decl)*
      (contents+=verilog_function)*
    '}'                                 #TaskVerilog
  ;

network
  : 'network' IDENTIFIER '{'
      (decls+=network_decl)*
      (contents+=network_content)*
    '}';

network_content
  : task
  | connect
  | instantiate
  | verilog_function
  ;

connect : lhs=dotted_name '->' rhs+=dotted_name (',' rhs+=dotted_name)* ';' ;

instantiate : IDENTIFIER '=' 'new' IDENTIFIER '(' param_args ')' ';' ;

known_type
  : 'bool'                    # BoolType
  | INTTYPE                   # IntType
  | UINTTYPE                  # UintType
  | IDENTIFIER                # IdentifierType
  | 'int'  '(' commaexpr ')'  # IntVType
  | 'uint' '(' commaexpr ')'  # UintVType
  ;

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

task_content
  : function
  | fence_function
  | verilog_function 
  ;

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

// Left value expressions
lval
  : ref=lval '[' idx=expr ']'                                   # LValIndex
  | ref=lval '[' lidx=expr op=(':' | '-:' | '+:') ridx=expr ']' # LValSlice
  | ref=lval '.' IDENTIFIER                                     # LValDot
  | IDENTIFIER                                                  # LValId
  | '{' lval (',' lval)+ '}'                                    # LValCat
  ;

commaexpr : (expr)? (',' expr)* ;

param_args : (param_assign (',' param_assign)*)? ;

param_assign : IDENTIFIER '=' expr ;

dotted_name : (es+=IDENTIFIER) ('.' es+=IDENTIFIER)*;

field : known_type IDENTIFIER SEMICOLON;

case_stmt
  : 'default' ':' statement # DefaultCase
  | commaexpr ':' statement # NormalCase
  ;

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
