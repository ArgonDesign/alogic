////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Any pass relying on Tree.id may fail with structural sharing. This pass
// copies any shared nodes.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class RemoveStructuralSharing extends StatelessTreeTransformer {

  // Set of stack symbols to replace
  private val visited = mutable.Set[Int]()

  override def transform(tree: Tree): Tree = {
    if (!(visited contains tree.id)) {
      visited add tree.id
      tree
    } else {
      // $COVERAGE-OFF$ Not all of these are necessarily used, but it is hard
      // to reason about what is and what isn't actually needed, plus they are
      // trivial so keep it a complete match with a coverage exclusion.
      val duplicate = tree match {
        case node: Ident => node.copy()
        case node: Sym   => node.copy()

        case node: DescVar          => node.copy()
        case node: DescVal          => node.copy()
        case node: DescStatic       => node.copy()
        case node: DescIn           => node.copy()
        case node: DescOut          => node.copy()
        case node: DescPipeVar      => node.copy()
        case node: DescPipeIn       => node.copy()
        case node: DescPipeOut      => node.copy()
        case node: DescParam        => node.copy()
        case node: DescParamType    => node.copy()
        case node: DescConst        => node.copy()
        case node: DescArray        => node.copy()
        case node: DescSram         => node.copy()
        case node: DescType         => node.copy()
        case node: DescEntity       => node.copy()
        case node: DescRecord       => node.copy()
        case node: DescInstance     => node.copy()
        case node: DescSingleton    => node.copy()
        case node: DescFunc         => node.copy()
        case node: DescPackage      => node.copy()
        case node: DescGenVar       => node.copy()
        case node: DescGenIf        => node.copy()
        case node: DescGenFor       => node.copy()
        case node: DescGenRange     => node.copy()
        case node: DescGenScope     => node.copy()
        case node: DescAlias        => node.copy()
        case node: DescParametrized => node.copy()

        case node: AttrBool => node.copy()
        case node: AttrExpr => node.copy()

        case node: GenCase => node.copy()

        case node: DeclVar       => node.copy()
        case node: DeclVal       => node.copy()
        case node: DeclStatic    => node.copy()
        case node: DeclIn        => node.copy()
        case node: DeclOut       => node.copy()
        case node: DeclPipeVar   => node.copy()
        case node: DeclPipeIn    => node.copy()
        case node: DeclPipeOut   => node.copy()
        case node: DeclConst     => node.copy()
        case node: DeclArray     => node.copy()
        case node: DeclSram      => node.copy()
        case node: DeclStack     => node.copy()
        case node: DeclType      => node.copy()
        case node: DeclEntity    => node.copy()
        case node: DeclRecord    => node.copy()
        case node: DeclInstance  => node.copy()
        case node: DeclSingleton => node.copy()
        case node: DeclFunc      => node.copy()
        case node: DeclState     => node.copy()

        case node: DefnVar       => node.copy()
        case node: DefnVal       => node.copy()
        case node: DefnStatic    => node.copy()
        case node: DefnIn        => node.copy()
        case node: DefnOut       => node.copy()
        case node: DefnPipeVar   => node.copy()
        case node: DefnPipeIn    => node.copy()
        case node: DefnPipeOut   => node.copy()
        case node: DefnConst     => node.copy()
        case node: DefnArray     => node.copy()
        case node: DefnSram      => node.copy()
        case node: DefnStack     => node.copy()
        case node: DefnType      => node.copy()
        case node: DefnEntity    => node.copy()
        case node: DefnRecord    => node.copy()
        case node: DefnInstance  => node.copy()
        case node: DefnSingleton => node.copy()
        case node: DefnFunc      => node.copy()
        case node: DefnState     => node.copy()

        case node: ImportOne     => node.copy()
        case node: ImportPending => node.copy()

        case node: UsingOne     => node.copy()
        case node: UsingAll     => node.copy()
        case node: UsingGenBody => node.copy()

        case node: FromOne => node.copy()
        case node: FromAll => node.copy()

        case node: AssertionAssert      => node.copy()
        case node: AssertionAssume      => node.copy()
        case node: AssertionStatic      => node.copy()
        case node: AssertionUnreachable => node.copy()

        case node: PkgSplice  => node.copy()
        case node: PkgCompile => node.copy()

        case node: EntSplice         => node.copy()
        case node: EntConnect        => node.copy()
        case node: EntAssign         => node.copy()
        case node: EntCombProcess    => node.copy()
        case node: EntClockedProcess => node.copy()
        case node: EntVerbatim       => node.copy()
        case node: EntComment        => node.copy()

        case node: RecSplice  => node.copy()
        case node: RecComment => node.copy()

        case node: StmtSplice  => node.copy()
        case node: StmtBlock   => node.copy()
        case node: StmtIf      => node.copy()
        case node: StmtCase    => node.copy()
        case node: StmtLoop    => node.copy()
        case node: StmtWhile   => node.copy()
        case node: StmtFor     => node.copy()
        case node: StmtDo      => node.copy()
        case node: StmtLet     => node.copy()
        case _: StmtFence      => StmtFence()
        case _: StmtBreak      => StmtBreak()
        case _: StmtContinue   => StmtContinue()
        case node: StmtGoto    => node.copy()
        case node: StmtReturn  => node.copy()
        case node: StmtAssign  => node.copy()
        case node: StmtUpdate  => node.copy()
        case node: StmtPost    => node.copy()
        case node: StmtDelayed => node.copy()
        case node: StmtOutcall => node.copy()
        case node: StmtExpr    => node.copy()
        case node: StmtWait    => node.copy()
        case node: StmtComment => node.copy()

        case node: CaseSplice  => node.copy()
        case node: CaseRegular => node.copy()
        case node: CaseDefault => node.copy()

        case node: ExprCall    => node.copy()
        case node: ExprBuiltin => node.copy()
        case node: ExprUnary   => node.copy()
        case node: ExprBinary  => node.copy()
        case node: ExprCond    => node.copy()
        case node: ExprRep     => node.copy()
        case node: ExprCat     => node.copy()
        case node: ExprIndex   => node.copy()
        case node: ExprSlice   => node.copy()
        case node: ExprDot     => node.copy()
        case node: ExprSel     => node.copy()
        case node: ExprSymSel  => node.copy()
        case node: ExprIdent   => node.copy()
        case node: ExprSym     => node.copy()
        case node: ExprOld     => node.copy()
        case node: ExprThis    => node.copy()
        case node: ExprType    => node.copy()
        case node: ExprCast    => node.copy()
        case node: ExprInt     => node.copy()
        case node: ExprNum     => node.copy()
        case node: ExprStr     => node.copy()

        case node: ArgP => node.copy()
        case node: ArgN => node.copy()
        case node: ArgD => node.copy()

        case _: Thicket => unreachable
        case Stump      => unreachable
      }
      // $COVERAGE-ON$
      duplicate withLoc tree.loc withTpe tree.tpe
    }
  }

}

object RemoveStructuralSharing extends PairTransformerPass {
  val name = "remove-structural-sharing"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new RemoveStructuralSharing
    (transformer(decl), transformer(defn))
  }

}
