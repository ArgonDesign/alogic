////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

// Handle nested fsms in network

package alogic

import alogic.ast._
import alogic.ast.AstOps._

object MakeStages {
  def apply(net: NetworkTask): Option[(NetworkTask, List[FsmTask])] = {
    val NetworkTask(name, decls, insts, conns, vfns, fsms) = net

    val stageNames = fsms map { _.name }

    val stages = for (FsmTask(sub, decls, fns, fencefn, vfns) <- fsms) yield {
      FsmTask(s"${name}__${sub}", decls, fns, fencefn, vfns)
    }

    val newInsts = for (inst <- insts) yield inst rewrite {
      case Instantiate(id, module, args) if stageNames contains module => Instantiate(id, s"${name}__${module}", args)
    }

    val network = NetworkTask(name, decls, newInsts, conns, vfns, fsms)

    Some((network, stages))
  }
}
