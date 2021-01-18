////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Complete compiler frontend, including parsing, elaboration and type
//  checking.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.util.unreachable

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.util.chaining._

class Frontend(implicit cc: CompilerContext) {

  implicit private val fe: Frontend = this

  private var d = 0

  private def withLog[T](msg: String)(f: => T): T = {
    d += 1
//    println("|" * d + " " + msg)
    val r = f
    d -= 1
    r
  }

  sealed trait PendingFrontendOp
  case class PendingTypeOf(symbol: Symbol) extends PendingFrontendOp
  case class PendingEvaluate(symbol: Symbol) extends PendingFrontendOp
  case class PendingSpecialzie(symbol: Symbol, params: List[Arg]) extends PendingFrontendOp
  case class PendingImport(paths: List[String], parts: List[String]) extends PendingFrontendOp

  private val pendingSet = mutable.Set[PendingFrontendOp]()
  private val pendingStack = mutable.Stack[(PendingFrontendOp, Loc)]()

  def guardCircular[T](op: PendingFrontendOp, loc: Loc)(f: => FinalResult[T]): FinalResult[T] = {
    if (pendingSet contains op) {
      val ms = new ListBuffer[Message]()
      def addNote(op: PendingFrontendOp, loc: Loc, initial: Boolean): Unit = {
        ms addOne {
          op pipe {
            case PendingTypeOf(symbol)   => s"type of symbol '${symbol.name}'"
            case PendingEvaluate(symbol) => s"value of symbol '${symbol.name}'"
            case PendingSpecialzie(symbol, params) =>
              val paramString = params map {
                case ArgP(expr)       => expr.toSource
                case ArgN(name, expr) => s"$name = ${expr.toSource}"
                case ArgD(base, idxs, expr) =>
                  s"$base#[${idxs map { _.toSource } mkString ","}] = ${expr.toSource}"
              } mkString ", "
              s"specialization of symbol '${symbol.name}' with parameters: $paramString"
            case PendingImport(_, parts) => s"imported package '${parts.mkString(".")}'"
          } pipe { msg =>
            if (initial) {
              Error(loc, s"Circular definition: $msg")
            } else {
              Note(loc, s"Depends on $msg")
            }
          }
        }
      }
      val (cycle, rest) = pendingStack.toList.span(_._1 != op)
      addNote(op, rest.head._2, true)
      cycle
        .filterNot {
          _._1 pipe {
            case PendingTypeOf(symbol)        => Some(symbol)
            case PendingEvaluate(symbol)      => Some(symbol)
            case PendingSpecialzie(symbol, _) => Some(symbol)
            case _: PendingImport             => None
          } exists {
            _.desc match {
              case _: DescGenIf | _: DescGenFor | _: DescGenRange | _: DescGenScope => true
              case _                                                                => false
            }
          }
        }
        .reverse
        .foreach { case (op, loc) => addNote(op, loc, false) }
      addNote(op, loc, false)
      Failure(ms.toSeq)
    } else {
      pendingSet.add(op)
      pendingStack.push((op, loc))
      f tap { _ =>
        pendingStack.pop()
        pendingSet.remove(op)
      }
    }
  }

  private val importCache = mutable.Map[String, FinalResult[Symbol]]()

  private def imprt(
      searchPaths: List[Path],
      nameParts: List[String],
      loc: Loc,
      paramOpts: Option[List[Arg]]
    ): FinalResult[Symbol] =
    searchPaths.iterator.flatMap { searchPath =>
      def loop(path: Path, parts: List[String]): Option[File] = parts match {
        case Nil => unreachable
        case p :: Nil =>
          val subPath = path.resolve(p)
          val pkgFile = if (subPath.toFile.isDirectory) {
            subPath.resolve("package.alogic").toFile
          } else {
            path.resolve(p + ".alogic").toFile
          }
          Option.when(pkgFile.isFile)(pkgFile)
        case p :: ps =>
          val subPath = path.resolve(p)
          val pkgFile = subPath.resolve("package.alogic").toFile
          if (!subPath.toFile.isDirectory || !pkgFile.isFile) None else loop(subPath, ps)
      }
      loop(searchPath, nameParts)
    }.nextOption match {
      case None =>
        Failure {
          Error(loc, s"No package named '${nameParts.mkString(".")}'") :: {
            searchPaths map { path => Note(loc, s"Looked in: $path") }
          }
        }
      case Some(file) =>
        importCache.getOrElseUpdate(
          file.getCanonicalPath,
          fe.elaborate(Source(file)) pipe {
            case Left(ms)    => Failure(ms)
            case Right(desc) => Complete(desc)
          } flatMap { desc =>
            fe.typeCheck(desc) map { _ => desc.symbol }
          }
        ) flatMap { symbol =>
          paramOpts match {
            case None => Complete(symbol)
            case Some(params) =>
              fe.typeOf(symbol, loc) flatMap {
                case t if t.isParametrized => fe.specialize(symbol, params, loc)
                case _                     => Failure(loc, "Package does not take parameters")
              }
          }
        }
    }

  def imprt(expr: Expr, relative: Int, loc: Loc): FinalResult[Symbol] =
    withLog(s"Importing $expr") {
      require(!expr.loc.isSynthetic)
      require(relative >= 0)
      val searchPaths = if (relative == 0) {
        cc.settings.importSearchDirs
      } else {
        @tailrec
        def loop(path: Path, n: Int): Path = if (n == 0) path else loop(path.getParent, n - 1)
        List(loop(Paths.get(expr.loc.file).toAbsolutePath, relative))
      }

      def extractParts(expr: Expr): FinalResult[(List[String], Option[List[Arg]])] = {
        def loop(expr: Expr): FinalResult[List[String]] = expr match {
          case ExprIdent(Ident(name, Nil)) => Complete(name :: Nil)
          case ExprDot(expr, name, Nil)    => loop(expr) map { name :: _ }
          case _                           => Failure(expr, "Invalid import expression")
        }
        expr match {
          case ExprCall(tgt, args) => loop(tgt) map { (_, Some(args)) }
          case _                   => loop(expr) map { (_, None) }
        }
      }
      extractParts(expr) flatMap {
        case (nameParts, paramOpts) =>
          guardCircular(PendingImport(searchPaths map { _.toString }, nameParts), loc) {
            imprt(searchPaths, nameParts.reverse, loc, paramOpts)
          }
      }
    }

  def typeCheck(tree: Tree): FinalResult[Type] =
    withLog(s"Type checking $tree") {
      TypeChecker(tree)
    }

  def typeOf(symbol: Symbol, loc: Loc, refresh: Boolean = false): FinalResult[Type] =
    withLog(s"Computing type of '$symbol'") {
      guardCircular(PendingTypeOf(symbol), loc) {
        TypeOf(symbol, refresh)
      }
    }

  def evaluate(expr: Expr, hint: => String): FinalResult[BigInt] =
    withLog(s"Computing value of $expr") {
      Evaluate(expr, hint)
    }

  def evaluate(
      symbol: Symbol,
      loc: Loc,
      hint: => String,
      markUsed: Boolean = true,
      paramCheck: Boolean = false
    ): FinalResult[Expr] =
    withLog(s"Computing value of '$symbol'") {
      guardCircular(PendingEvaluate(symbol), loc) {
        Evaluate(symbol, loc, hint, markUsed, paramCheck)
      }
    }

  def nameFor(base: String, idxs: List[Expr]): FinalResult[String] =
    if (idxs.isEmpty) {
      Complete(base)
    } else {
      idxs
        .map(evaluate(_, "Identifier index"))
        .distil
        .map(_ mkString (base + "#[", ",", "]"))
    }

  def specialize(symbol: Symbol, params: List[Arg], loc: Loc): FinalResult[Symbol] =
    withLog(s"Specializing '${symbol.name}' with $params") {
      guardCircular(PendingSpecialzie(symbol, params), loc) {
        Specialize(symbol, params, loc)
      }
    }

  def elaborate[T <: Tree: ListElaborable](
      trees: List[T],
      symtab: SymbolTable,
      paramsOpt: Option[(Loc, List[Arg])]
    ): Result[List[T]] =
    Elaborate.list[T](trees, symtab, paramsOpt)

  def elaborate(
      source: Source
    )(
      implicit
      cc: CompilerContext
    ): Either[Seq[Message], Desc] =
    Some(source) flatMap {
      implicit val mb: MessageBuffer = cc.messageBuffer
      Parser[DescPackage](_, SourceContext.Package) // Parse the file
    } flatMap {
      SyntaxCheck(_) // Run SyntaxChecks
//    } map { t =>
//      println(t.toSource); t
    } flatMap {
      SyntaxNormalize(_) // Normalize some constructs for easier elaboration
//    } map { t =>
//      println(t.toSource); t
    } map {
      case desc: DescPackage =>
        // Elaborate the package definition
        @tailrec
        def loop(desc: Desc): FinalResult[Desc] = Elaborate(desc, cc.builtins) match {
          case Finished(_)            => unreachable
          case Partial(d, _)          => loop(d)
          case complete @ Complete(_) => complete
          case unknown: Unknown       => unknown
          case failure: Failure       => failure
        }
        loop(desc).toEither
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        // Parametrized package, will definitely not need to iterate just yet
        Elaborate(desc, cc.builtins) match {
          case Complete(value) => Right(value)
          case _               => unreachable
        }
      case _ => unreachable
    } pipe {
      case None        => Left(Nil)
      case Some(other) => other
    }

  def finalize(
      desc: DescPackage
    )(
      implicit
      cc: CompilerContext
    ): Option[(DescPackage, Iterable[DescPackage])] = {
    // Apply Finalize
    val finalized @ (input, dependencies) = Finalize(desc)
    //
    val iterable = (Iterable(input) concat dependencies.iterator).par
    // Apply a secondary SyntaxCheck to ensure 'gen' yielded well formed trees
    iterable foreach { SyntaxCheck(_) }
    // Apply UnusedCheck
    iterable foreach UnusedCheck.apply
    //
    Some(finalized)
  }

  def apply(
      source: Source,
      loc: Loc,
      params: List[Arg]
    )(
      implicit
      cc: CompilerContext
    ): Option[(DescPackage, Iterable[DescPackage])] =
    elaborate(source) flatMap { desc =>
      typeCheck(desc).toEither map { _ => desc }
    } pipe {
      case Left(ms)    => ms foreach cc.addMessage; None
      case Right(desc) => Some(desc)
    } flatMap {
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        elaborate(params, cc.builtins, None)
          .pipe {
            case result: FinalResult[List[Arg]] => result
            case Finished(result)               => Complete(result)
            case Partial(_, rs)                 => Unknown(rs)
          }
          .flatMap { params =>
            specialize(desc.symbol, params, loc)
          }
          .toEither
          .pipe {
            case Left(ms)      => ms foreach cc.addMessage; None
            case Right(symbol) => Some(symbol.desc.asInstanceOf[DescPackage])
          }
      case desc: DescPackage =>
        if (params.isEmpty) {
          Some(desc)
        } else {
          val msg = s"Package defined in input file '${source.name}' does not take any parameters."
          params foreach { cc.error(_, msg) }
          None
        }
      case _ => unreachable

    } flatMap finalize

}
