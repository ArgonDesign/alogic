// We convert control statements into state labels and goto statements
//
// In the returned AST there will be no:
//   BreakStmt
//   ReturnStmt
//   ControlFor
//   ControlDo
//   ControlIf
//   ControlCaseStmt
//   WhileLoop 
//   FunCall
//   ControlBlock
//   FenceStmt
//   GotoStmt
//
// These will be replaced with 
//   GotoState
//   StateStmt
//   GotoStmt (this is used to target the callstack)

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

final class MakeStates {

  var state_num = 0;  // Track which state to use
  
  val fn2state = mutable.Map[String, Int]()  // First state in each function
  
  var breakTargets : List[Int] = Nil // Stack of targets when we see a break

  def apply(tree:Program) : Program = {
    tree.cmds.map(generateFunStates)  // Prepare mapping fn2state
    Program(tree.cmds.map(makeEntityStates))   // Transform tree
  }
  
  def generateFunStates(tree: AlogicAST) : Unit = tree match {
    case Task(t,n,decls,fns) => fns.map(generateFunStates)
    case x => 
  }
  
  def generateFunStates(tree: TaskContent) : Unit = tree match {
    case Function(name,body) => createFnState(name)
    case x => 
  }
  
  def makeEntityStates(tree: AlogicAST) : AlogicAST = tree match {
    case Task(t,n,decls,fns) => Task(t,n,decls,fns.map(makeFnStates))
    case x => x
  }
  
  def makeFnStates(tree: TaskContent) : TaskContent = tree match {
    case Function(name,body) => {
      val s = fn2state(name)
      val (current, extra) = makeStates(s,body)
      Function(name,CombinatorialBlock( StateStmt(s) :: current ::: extra ))
    }
    case x => x
  }
  
  def createFnState(name: String) : Int = {
    val s = state_num
    fn2state(name) = s
    state_num += 1
    s
  }
  
  val callDepth = DottedName(List("call_depth"))
  val callStack = DottedName(List("call_stack"))
  
  def makeNum(x: Int) = Num(s"$x")
  
  // Create an AST that finishes by moving to finalState
  // Returns two lists
  //   The first list is all the statements that should be appended to the current list (i.e. cmds in this state)
  //   The second list is additional self-contained statements that need to go somewhere
  //   Need to consider what would happen if the first commands were generated within a while statement
  def makeStates(finalState: Int, tree: AlogicAST) : (List[AlogicAST],List[AlogicAST]) = tree match {
    case FenceStmt() => (List(GotoState(finalState)), Nil)
    case BreakStmt() => (List(GotoState(breakTargets.head)), Nil)
    case ReturnStmt() => (List(
      Minusminus(callDepth),
      GotoStmt("call_stack[call_depth_nxt]")
    ), Nil)
    case GotoStmt(t) => (List(GotoState(fn2state(t))), Nil) // TODO check targets exist in AstBuilder to avoid exception here
    case FunCall(name,args) => (List( // require(args.length==0)  // TODO check in builder
      // Push return state
      Assign(ArrayLookup(callStack,callDepth),"=",makeNum(finalState)),
      // increment depth
      Plusplus(callDepth),
      // branch to function
      GotoState(fn2state(ExtractName(name))) // TODO check target exists in builder
    ), Nil)
    case ControlBlock(cmds) => makeBlockStmts(finalState, cmds)
    case WhileLoop(cond,body) => {
      val s = newState()
      val (follow,extra) = makeStates(finalState,body)
      (List(
        GotoState(s),
        StateStmt(s),
        CombinatorialIf(cond,CombinatorialBlock(follow),Some(GotoState(finalState)))
        ), extra)
    }
      
    // TODO
//   ControlFor
//   ControlDo
//   ControlIf
//   ControlCaseStmt


    case x => (x :: Nil, Nil)
  }
  
  def newState() : Int = {
    val s = state_num
    state_num += 1
    s
  }
  
  //  We need to split into control units
  //  A control unit is a set of combinatorial statements followed by a control statement
  //  We then have a new state for each intermediate control unit   
  def makeBlockStmts(finalState: Int, tree: List[AlogicAST] ) : (List[AlogicAST],List[AlogicAST])  = tree match {
    case Nil => (Nil, Nil)
    case x :: Nil => makeStates(finalState, x) 
    case x :: xs if (is_control_stmt(x)) => {
      val s = newState()
      val (current, extra) = makeStates(s,x)
      val (current2, extra2) = makeBlockStmts(finalState, xs)
      (current, StateStmt(s) :: current2 ::: extra ::: extra2) // TODO can we make this function tail recursive somehow?
    }
    case x :: xs => { // x must be combinatorial
      val (current, extra) = makeBlockStmts(finalState,xs)
      (x :: current, extra)
    }
  }
   

}
