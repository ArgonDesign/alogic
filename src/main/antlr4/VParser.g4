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
  | 'pipeline'  #PipelineType
  | 'verilog'   #VerilogType
  ;

decl_noinit
  : known_type var_ref            #DeclNoInit
  ;

decl_init
  : known_type var_ref '=' expr   #DeclInit
  ;

declaration
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

task_declaration
  : 'out' sync_type? known_type IDENTIFIER ';'            #TaskDeclOut
  | 'in' sync_type? known_type IDENTIFIER ';'             #TaskDeclIn
  | 'const' sync_type? known_type IDENTIFIER '=' expr ';' #TaskDeclConst
  | 'param' sync_type? known_type IDENTIFIER '=' expr ';' #TaskDeclParam
  | 'verilog' known_type var_ref ';'                      #TaskDeclVerilog
  | declaration ';'                                       #TaskDecl
  ;

task
  : tasktype IDENTIFIER '{'
      (decls+=task_declaration)*
      (contents+=task_content)* 
    '}';

network
  : 'network' IDENTIFIER '{'
      (decls+=task_declaration)*
       (contents+=network_content)*
    '}';

network_content
  : task
  | connect
  | instantiate
  ;

connect : dotted_name '->' comma_args ';' ;

instantiate : IDENTIFIER '=' IDENTIFIER '(' param_args ')' ';' ;

known_type
  : 'bool'                    # BoolType
  | INTTYPE                   # IntType
  | UINTTYPE                  # UintType
  | IDENTIFIER                # IdentifierType
  | 'struct' '{'
      (fields+=field)* 
    '}'                       # StructType
  | 'int'  '(' comma_args ')' # IntVType
  | 'uint' '(' comma_args ')' # UintVType
  ;

task_content
  : 'void' IDENTIFIER '(' ')' '{'
      (stmts += statement)*
    '}'                             # Function
  | 'void' 'fence' '(' ')' '{'
      (stmts += statement)*
    '}'                             # FenceFunction
  | VERILOGFUNC VERILOGBODY         # VerilogFunction
  ;

expr
  : '(' expr ')'                              # ExprBracket
  | op=('+' | '-' | '!' | '~' | '&' |
        '~&' | '|' | '~|' | '^' | '~^') expr  # ExprUnary
  | expr op='*' expr                          # ExprMulDiv  // TODO: '/' '%'
  | expr op=('+' | '-') expr                  # ExprAddSub
  | expr op=('<<' | '>>' | '>>>') expr        # ExprShift   // TODO: '<<<'
  | expr op=('>' | '>=' | '<' | '<=') expr    # ExprCompare
  | expr op=('==' | '!=') expr                # ExprEqual
  | expr op='&' expr                          # ExprBAnd
  | expr op=('^' | '~^') expr                 # ExprBXor
  | expr op='|' expr                          # ExprBOr
  | expr op='&&' expr                         # ExprAnd
  | expr op='||' expr                         # ExprOr
  | expr '?' expr ':' expr                    # ExprTernary
  | '{' expr '{' expr '}' '}'                 # ExprRep
  | '{' comma_args '}'                        # ExprCat
  | dotted_name '(' comma_args ')'            # ExprCall
  | var_ref                                   # ExprVarRef
  | DOLLARID '(' comma_args ')'               # ExprDollar
  | 'true'                                    # ExprTrue
  | 'false'                                   # ExprFalse
  | TICKNUM                                   # ExprTrickNum
  | CONSTANT TICKNUM                          # ExprConstTickNum
  | CONSTANT                                  # ExprConst
  | LITERAL                                   # ExprLiteral
  ;

var_ref
  : dotted_name '[' expr ']'                              # VarRefIndex
  | dotted_name '[' expr op=(':' | '-:' | '+:') expr ']'  # VarRefSlice
  | dotted_name                                           # VarRef
  ;

comma_args : (es+=expr)? (',' es+=expr)*;

param_args : (es+=paramAssign)? (',' es+=paramAssign)*;

paramAssign : expr '=' expr;

dotted_name : (es+=IDENTIFIER) ('.' es+=IDENTIFIER)*;

field : known_type IDENTIFIER SEMICOLON;

case_stmt
  : 'default'  ':' statement # DefaultCase
  | comma_args ':' statement # NormalCase
  ;

statement
  : '{' (stmts+=statement)* '}'                 # BlockStmt
  | declaration ';'                             # DeclStmt
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
  : var_ref                                   # LValue
  | '{' refs+=lvalue (',' refs+=lvalue)+ '}'  # LValueCat
  ;

assignment_statement
  : lvalue '++'           # AssignInc
  | lvalue '--'           # AssignDec
  | lvalue '=' expr       # Assign
  | lvalue ASSIGNOP expr  # AssignUpdate
  ;
