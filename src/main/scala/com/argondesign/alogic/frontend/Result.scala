////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees.ExprIdent
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Locatable
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbols.Symbol

// Result of a front-end operation
sealed trait Result[+T] extends ResultOps[T]

// Result of a front-end operation where partial progress is not possible
sealed trait FinalResult[+T] extends Result[T] with FinalResultOps[T]

// Finished represents the input was already processed, no progress made.
final case class Finished[+A](value: A) extends Result[A]

// Complete represents progress was made, and no further processing is needed.
final case class Complete[+A](value: A) extends FinalResult[A]

// Partial represents progress was made, but the result is not finished due to
// the given reasons.
final case class Partial[+A](value: A, rs: Seq[Reason]) extends Result[A] {
  require(rs.nonEmpty)
}

// Unknown represents no progress was made, due to the given reasons.
final case class Unknown(rs: Seq[Reason]) extends FinalResult[Nothing] {
  require(rs.nonEmpty)
}

// An error was encountered, re-attempting the operation is futile.
final case class Failure(ms: Seq[Message]) extends FinalResult[Nothing] {
//  require(ms.nonEmpty)
}

// Extractor for successful finished result, i.e.: Finished(_) or Complete(_)
object Success { //
  def unapply[T](result: Result[T]): Option[T] = result match {
    case Finished(value) => Some(value)
    case Complete(value) => Some(value)
    case _               => None
  } //
  def unapply[T](result: FinalResult[T]): Option[T] = result match {
    case Complete(value) => Some(value)
    case _               => None
  } //
}

// Extractor for result indicating progress, i.e.: Complete(_) or Partial(_)
object Progress { //
  def unapply[T](result: Result[T]): Option[T] = result match {
    case Complete(value)   => Some(value)
    case Partial(value, _) => Some(value)
    case _                 => None
  } //
}

// Factories
object Unknown {
  def apply(r: Reason): Unknown = Unknown(Seq(r))
}

object Failure {
  def apply(m: Message): Failure = Failure(Seq(m))

  def apply[T: Locatable](item: T, msg: String*): Failure = Failure(Error(item, msg: _*))
}

// Reasons for Unknown result
sealed trait Reason
case class ReasonUnresolved(expr: ExprIdent) // Name not in symbol table
    extends Reason
case class ReasonUnelaborated(tree: Tree) // Tree is not elaborated yet
    extends Reason
case class ReasonNeedsParamValue(symbol: Symbol, loc: Loc) // No actual parameter value
    extends Reason
case class ReasonEarlierTypeError(tree: Tree) // Encountered type error during earlier type check
    extends Reason

trait ResultOps[+T] { self: Result[T] =>

  // As you would expect, map value if present, otherwise leave unchanged
  def map[A](f: T => A): Result[A] = self match {
    // $COVERAGE-OFF$ convenience method, some cases might not be used
    case Finished(value)    => Finished(f(value))
    case Complete(value)    => Complete(f(value))
    case Partial(value, rs) => Partial(f(value), rs)
    case unknown: Unknown   => unknown
    case failure: Failure   => failure
    // $COVERAGE-ON$
  }

  // Apply f to value, if present
  final def tapEach(f: T => Unit): this.type = {
    this match {
      // $COVERAGE-OFF$ convenience method, some cases might not be used
      case Finished(value)   => f(value)
      case Complete(value)   => f(value)
      case Partial(value, _) => f(value)
      case _                 =>
      // $COVERAGE-ON$
    }
    this
  }

  final def contains[A >: T](elem: A): Boolean = self match {
    // $COVERAGE-OFF$ convenience method, some cases might not be used
    case Finished(value)   => value == elem
    case Complete(value)   => value == elem
    case Partial(value, _) => value == elem
    case _: Unknown        => false
    case _: Failure        => false
    // $COVERAGE-ON$
  }

  final def mapFailing[A](f: T => Either[Seq[Message], A]): Result[A] = self match {
    // $COVERAGE-OFF$ convenience method, some cases might not be used
    case Finished(value)    => f(value).fold(Failure(_), Finished(_))
    case Complete(value)    => f(value).fold(Failure(_), Complete(_))
    case Partial(value, rs) => f(value).fold(Failure(_), Partial(_, rs))
    case unknown: Unknown   => unknown
    case failure: Failure   => failure
    // $COVERAGE-ON$
  }

  // This is similar to flatMap but the result type is constrained to be
  // compatible with the type of this result, hence we can preserve progress
  // when the mapping yields an unknown/more complete result.
  final def proceed[A >: T](f: T => Result[A]): Result[A] = self match {
    case Finished(value) =>
      f(value) // This was finished, yield whatever the mapping yields
    case Complete(value) =>
      f(value) match {
        case Finished(newValue) => Complete(newValue) // Demote to complete
        case Unknown(rs)        => Partial(value, rs) // Promote to partial
        case other              => other
      }
    case partial @ Partial(_, _) => partial
    case unknown: Unknown        => unknown
    case failure: Failure        => failure
  }

}

trait FinalResultOps[+T] { self: FinalResult[T] =>

  final override def map[A](f: T => A): FinalResult[A] = self match {
    case Complete(value)  => Complete(f(value))
    case unknown: Unknown => unknown
    case failure: Failure => failure
  }

  // flatMap only works on FinalResult, as otherwise it might loose
  // progress, e.g.: Partial[A](_) flatMap[Result[B]] { _ => Unknown(_) }
  // cannot be reconciled as the type of the new result is not compatible
  // with the type of this result
  final def flatMap[A](f: T => FinalResult[A]): FinalResult[A] = self match {
    case Complete(value)  => f(value)
    case unknown: Unknown => unknown
    case failure: Failure => failure
  }

  final def flatMap[A](f: T => Result[A]): Result[A] = self match {
    case Complete(value)  => f(value)
    case unknown: Unknown => unknown
    case failure: Failure => failure
  }

  final def get: T = self match {
    case Complete(value) => value
    case _: Unknown      => throw Ice(".get on Unknown result")
    case _: Failure      => throw Ice(".get on Failure result")
  }

  final def toEither(implicit fe: Frontend): Either[Seq[Message], T] = self match {
    case Complete(r) => Right(r)
    case Unknown(rs) =>
      Left {
        val (userReasons, nonUserReasons) = rs partition {
          case _: ReasonUnelaborated => false
          case _                     => true
        }

        val reasons = if (userReasons.nonEmpty) {
          userReasons
        } else {
          // $COVERAGE-OFF$ There should not be internal compiler errors
          nonUserReasons
          // $COVERAGE-ON$
        }

        reasons flatMap {
          case ReasonUnresolved(t) =>
            fe.nameFor(t.base, t.idxs) match {
              case Complete(name) => Iterator.single(Error(t, s"'$name' is undefined"))
              case _              => Iterator.single(Error(t, s"identifier is undefined"))
            }

          case ReasonNeedsParamValue(symbol, loc) =>
            Iterator(
              Error(loc, s"'${symbol.name}' requires actual parameter value"),
              Note.definedHere(symbol.desc)
            )
          // $COVERAGE-OFF$ ICEs should not be hit..
          case ReasonUnelaborated(t)     => Iterator.single(Ice(t, s"Not elaborated"))
          case ReasonEarlierTypeError(t) => Iterator.single(Ice(t, s"Earlier type error"))
          // $COVERAGE-ON$
        }
      }
    case Failure(ms) => Left(ms.distinct)
  }

}

object FinalResult {

  implicit class ListFinalResultOps[T](val self: List[FinalResult[T]]) extends AnyVal {

    def distil: FinalResult[List[T]] =
      self.reverse.foldLeft[FinalResult[List[T]]](Complete(Nil)) {
        case (Failure(acc), Failure(ms)) => Failure(ms concat acc) // Accumulate messages
        case (_, failure: Failure)       => failure // First failure
        case (failure: Failure, _)       => failure // Earlier failure
        case (Unknown(acc), Unknown(us)) => Unknown(us concat acc) // Accumulate reasons
        case (_, unknowns: Unknown)      => unknowns // First unknown
        case (unknowns: Unknown, _)      => unknowns // Earlier unknown
        case (Complete(ts), Complete(t)) => Complete(t :: ts)
      }

  }

}
