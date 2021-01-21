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
  case class PendingImport(path: String) extends PendingFrontendOp

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
            case PendingImport(path) => s"""imported file "$path""""
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
      path: String,
      loc: Loc,
      absolute: Boolean
    ): FinalResult[Symbol] =
    searchPaths.iterator.flatMap { searchPath =>
      val asIs = searchPath.resolve(path).toFile
      Option.when(asIs.isFile)(asIs) orElse {
        val withSuffix = searchPath.resolve(path + ".alogic").toFile
        Option.when(withSuffix.isFile)(withSuffix)
      }
    }.nextOption match {
      case None =>
        Failure {
          if (absolute) {
            Error(loc, s"""Cannot find absolute import target "$path"""") :: {
              searchPaths map { path => Note(loc, s"Looked in: $path") }
            }
          } else {
            Error(loc, s"""Cannot find relative import target "$path"""") :: {
              val expected = searchPaths.head.resolve(path).toFile.getCanonicalPath
              val msg = s"Path does not exist or is not a regular file: $expected" :: {
                if (path endsWith ".alogic") Nil else List(s"or: $expected.alogic")
              }
              Note(loc, msg) :: Nil
            }
          }
        }
      case Some(file) =>
        importCache.getOrElseUpdate(
          file.getCanonicalPath,
          // TODO: sandbox check
          guardCircular(PendingImport(file.getCanonicalPath), loc) {
            file.getName.dropWhile(_ != '.') match {
              case ".alogic" =>
                fe.elaborate(Source(file)) pipe {
                  case Left(ms)    => Failure(ms)
                  case Right(desc) => Complete(desc)
                } flatMap { desc =>
                  fe.typeCheck(desc) map { _ => desc.symbol }
                }
              case _ =>
                Failure(
                  loc,
                  s"Unable to import file ${file.getCanonicalPath}",
                  "unknown filename extension"
                )
            }
          }
        ) flatMap { symbol =>
          Complete(symbol)
        }
    }

  def imprt(path: String, loc: Loc): FinalResult[Symbol] =
    withLog(s"""Importing "$path"""") {
      path.trim pipe { path =>
        if (path.isEmpty) {
          Failure(loc, "Empty import path")
        } else if (path.startsWith("/")) {
          Failure(loc, "Import path cannot start with a leading '/'")
        } else {
          val absolute = path.head != '.'
          val searchPaths = if (absolute) {
            // Absolute import
            cc.settings.importSearchDirs
          } else {
            // Relative import
            List(Paths.get(loc.file).toAbsolutePath.getParent)
          }
          imprt(searchPaths, path, loc, absolute)
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
