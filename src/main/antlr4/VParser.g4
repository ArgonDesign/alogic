parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start : (entities+=entity)* EOF ;

entity
  : typedef
  | task
  | network
  ;

typedef : 'typedef' known_type IDENTIFIER ';' ;

tasktype
  : 'fsm'       #FsmType
  | 'verilog'   #VerilogType
  ;

decl_noinit
  : known_type var_ref            #DeclNoInit
  ;

decl_init
  : known_type var_ref '=' expr   #DeclInit
  ;

decl
  : decl_noinit
  | decl_init
  ;

sync_type
  : SYNC_READY_BUBBLE #SyncReadyBubbleType
  | WIRE_SYNC_ACCEPT  #WireSyncAcceptType
  | SYNC_READY        #SyncReadyType
  | WIRE_SYNC         #WireSyncType
  | SYNC_ACCEPT       #SyncAcceptType
  | SYNC              #SyncType
  | WIRE              #WireType
  ;

network_decl
  : 'out' sync_type? known_type IDENTIFIER ';'  #TaskDeclOut
  | 'in' sync_type? known_type IDENTIFIER ';'   #TaskDeclIn
  | 'param' known_type IDENTIFIER '=' expr ';'  #TaskDeclParam
  | 'const' known_type IDENTIFIER '=' expr ';'  #TaskDeclConst
  ;

task_decl
  : network_decl                        #DUMMYRULENAME
  | 'verilog' known_type var_ref ';'    #TaskDeclVerilog
  | decl ';'                            #TaskDecl
  ;

task
  : tasktype IDENTIFIER '{'
      (decls+=task_decl)*
      (contents+=task_content)*
    '}';

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

connect : dotted_name '->' commaexpr ';' ;

instantiate : IDENTIFIER '=' 'new' IDENTIFIER '(' param_args ')' ';' ;

known_type
  : 'bool'                    # BoolType
  | INTTYPE                   # IntType
  | UINTTYPE                  # UintType
  | IDENTIFIER                # IdentifierType
  | 'struct' '{'
      (fields+=field)*
    '}'                       # StructType
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

expr
  : '(' expr ')'                                      # ExprBracket
  | op=('+' | '-' | '!' | '~' | '&' |
        '~&' | '|' | '~|' | '^' | '~^') expr          # ExprUnary
  | expr op=('*' | '/' | '%') expr                    # ExprMulDiv
  | expr op=('+' | '-') expr                          # ExprAddSub
  | expr op=('<<' | '>>' | '>>>') expr                # ExprShift   // TODO: '<<<'
  | expr op=('>' | '>=' | '<' | '<=') expr            # ExprCompare
  | expr op=('==' | '!=') expr                        # ExprEqual
  | expr op='&' expr                                  # ExprBAnd
  | expr op=('^' | '~^') expr                         # ExprBXor
  | expr op='|' expr                                  # ExprBOr
  | expr op='&&' expr                                 # ExprAnd
  | expr op='||' expr                                 # ExprOr
  | expr '?' expr ':' expr                            # ExprTernary
  | '{' expr '{' expr '}' '}'                         # ExprRep
  | '{' commaexpr '}'                                 # ExprCat
  | dotted_name '(' commaexpr ')'                     # ExprCall
  | var_ref                                           # ExprVarRef
  | var_ref '[' expr op=(':' | '-:' | '+:') expr ']'  # ExprSlice
  | DOLLARID '(' commaexpr ')'                        # ExprDollar
  | 'true'                                            # ExprTrue
  | 'false'                                           # ExprFalse
  | TICKNUM                                           # ExprTrickNum
  | CONSTANT TICKNUM                                  # ExprConstTickNum
  | CONSTANT                                          # ExprConst
  | LITERAL                                           # ExprLiteral
  ;

var_ref
  : dotted_name ('[' es+=expr ']')+   # VarRefIndex
  | dotted_name                       # VarRef
  ;

commaexpr : (expr)? (',' expr)* ;

param_args : (es+=paramAssign)? (',' es+=paramAssign)*;

paramAssign : expr '=' expr;

dotted_name : (es+=IDENTIFIER) ('.' es+=IDENTIFIER)*;

field : known_type IDENTIFIER SEMICOLON;

case_stmt
  : 'default' ':' statement # DefaultCase
  | commaexpr ':' statement # NormalCase
  ;

statement
  : '{' (stmts+=statement)* '}'                 # BlockStmt
  | decl ';'                                    # DeclStmt
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
  | decl_init             #ForInitDecl
  ;

lvalue
  : var_ref                                             # LValue
  | var_ref '[' expr op=(':' | '-:' | '+:') expr ']'    # LValueSlice
  | '{' refs+=lvalue (',' refs+=lvalue)+ '}'            # LValueCat
  ;

assignment_statement
  : lvalue '++'           # AssignInc
  | lvalue '--'           # AssignDec
  | lvalue '=' expr       # Assign
  | lvalue ASSIGNOP expr  # AssignUpdate
  ;
