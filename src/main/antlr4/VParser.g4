parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start : (entities+=entity)* EOF ;

entity :
   typedef
 | task
 | network
 ;

typedef : TYPEDEF known_type IDENTIFIER SEMICOLON;

tasktype : FSM #FsmType
  | PIPELINE   #PipelineType
  | VERILOG    #VerilogType
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

sync_type : SYNC_READY_BUBBLE #SyncReadyBubbleType
          | WIRE_SYNC_ACCEPT  #WireSyncAcceptType
          | SYNC_READY #SyncReadyType
          | WIRE_SYNC #WireSyncType
          | SYNC_ACCEPT #SyncAcceptType
          | SYNC #SyncType
          | WIRE #WireType
          ;

task_declaration
  : 'out' sync_type? known_type IDENTIFIER ';'          #TaskDeclOut
  | 'in' sync_type? known_type IDENTIFIER ';'           #TaskDeclIn
  | PARAM sync_type? known_type IDENTIFIER '=' expr ';' #TaskDeclParam
  | 'verilog' known_type var_ref ';'                    #TaskDeclVerilog
  | declaration ';'                                     #TaskDecl
  ;

task : tasktype IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=task_content)* RIGHTCURLY;

network : NETWORK IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=network_content)* RIGHTCURLY;

network_content : task | connect | instantiate;

connect : dotted_name GOESTO comma_args SEMICOLON;

instantiate : IDENTIFIER EQUALS IDENTIFIER LEFTBRACKET param_args RIGHTBRACKET SEMICOLON;

known_type :
  BOOL                                            # BoolType
  | INTTYPE                                       # IntType
  | UINTTYPE                                      # UintType
  | IDENTIFIER                                    # IdentifierType
  | STRUCT LEFTCURLY (fields+=field)* RIGHTCURLY  # StructType
  | INT LEFTBRACKET comma_args RIGHTBRACKET       # IntVType
  | UINT LEFTBRACKET comma_args RIGHTBRACKET      # UintVType
  ;

task_content
  : 'void' IDENTIFIER '(' ')' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY                                    # Function
  | 'void' 'fence' '(' ')' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY                                    # FenceFunction
  | VERILOGFUNC VERILOGBODY                       # VerilogFunction
  ;

expr
  : '(' expr ')'                                          # ExprBracket
  | op=('+' | '-' | '!' | '~' | '&' |
        '~&' | '|' | '~|' | '^' | '~^') expr              # ExprUnary
  | expr op='*' expr                                      # ExprMulDiv  // TODO: '/' '%'
  | expr op=('+' | '-') expr                              # ExprAddSub
  | expr op=('<<' | '>>' | '>>>') expr                    # ExprShift   // TODO: '<<<'
  | expr op=('>' | '>=' | '<' | '<=') expr                # ExprCompare
  | expr op=('==' | '!=') expr                            # ExprEqual
  | expr op='&' expr                                      # ExprBAnd
  | expr op=('^' | '~^') expr                             # ExprBXor
  | expr op='|' expr                                      # ExprBOr
  | expr op='&&' expr                                     # ExprAnd
  | expr op='||' expr                                     # ExprOr
  | expr '?' expr ':' expr                                # ExprTernary
  | LEFTCURLY expr LEFTCURLY expr RIGHTCURLY RIGHTCURLY   # ExprRep
  | LEFTCURLY comma_args RIGHTCURLY                       # ExprCat
  | dotted_name '(' comma_args ')'                        # ExprCall
  | var_ref                                               # ExprVarRef
  | DOLLARID '(' comma_args ')'                           # ExprDollar
  | 'true'                                                # ExprTrue
  | 'false'                                               # ExprFalse
  | TICKNUM                                               # ExprTrickNum
  | CONSTANT TICKNUM                                      # ExprConstTickNum
  | CONSTANT                                              # ExprConst
  | LITERAL                                               # ExprLiteral
  ;

var_ref
  : dotted_name '[' expr ']'                              # VarRefIndex
  | dotted_name '[' expr op=(':' | '-:' | '+:') expr ']'  # VarRefSlice
  | dotted_name                                           # VarRef
  ;

comma_args : (es+=expr)? (COMMA es+=expr)*;

param_args : (es+=paramAssign)? (COMMA es+=paramAssign)*;

paramAssign : expr EQUALS expr;

dotted_name : (es+=IDENTIFIER) ('.' es+=IDENTIFIER)*;

field : known_type IDENTIFIER SEMICOLON;


case_stmt :
  DEFAULT COLON statement # DefaultCase
  | comma_args COLON statement # NormalCase
  ;

statement
  : LEFTCURLY (stmts+=statement)* RIGHTCURLY                # BlockStmt
  | declaration ';'                                         # DeclStmt
  | 'while' '(' expr ')' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY                                              # WhileStmt
  | 'if' '(' expr ')'
      thenStmt=statement
    ('else'
      elseStmt=statement)?                                  # IfStmt
  | 'case' '(' expr ')' LEFTCURLY
      (cases+=case_stmt)+
    RIGHTCURLY                                              # CaseStmt
  | 'for' '(' init=for_init ';'
              cond=expr ';'
              step=assignment_statement ')' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY                                              # ForStmt
  | 'do' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY 'while' '(' expr ')' ';'                     # DoStmt
  | 'fence' ';'                                             # FenceStmt
  | 'break' ';'                                             # BreakStmt
  | 'return' ';'                                            # ReturnStmt
  | '$' '(' LITERAL ')' ';'                                 # DollarCommentStmt
  | 'goto' IDENTIFIER ';'                                   # GotoStmt
  | assignment_statement ';'                                # AssignmentStmt
  | expr ';'                                                # ExprStmt
  ;

for_init
  : assignment_statement  #ForInitNoDecl
  | decl_init             #ForInitDecl
  ;

lvalue
  : var_ref                                                 # LValue
  | LEFTCURLY refs+=lvalue (',' refs+=lvalue)+ RIGHTCURLY   # LValueCat
  ;

assignment_statement
  : lvalue '++'                              # AssignInc
  | lvalue '--'                              # AssignDec
  | lvalue '=' expr                          # Assign
  | lvalue ASSIGNOP expr                     # AssignUpdate
  ;
