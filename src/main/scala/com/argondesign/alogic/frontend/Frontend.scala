////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Complete compiler frontend, including parsing, elaboration and type
//  checking.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.builtins.Builtins
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.ParOrSeqIterable
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.util.unreachable

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.chaining._

sealed trait PendingFrontendOp
case class PendingTypeOf(symbol: Symbol) extends PendingFrontendOp
case class PendingEvaluate(symbol: Symbol) extends PendingFrontendOp
case class PendingSpecialzie(symbol: Symbol, params: List[Arg]) extends PendingFrontendOp
case class PendingImport(path: String) extends PendingFrontendOp

final class Frontend private (
    private val importCache: TrieMap[String, () => Either[FinalResult[Symbol], Future[
      FinalResult[Symbol]
    ]]],
    private val pendingSet: mutable.Set[PendingFrontendOp],
    private val pendingStack: mutable.Stack[(PendingFrontendOp, Loc)]
  )(
    implicit
    cc: CompilerContext) {

  def this()(implicit cc: CompilerContext) = this(TrieMap(), mutable.Set(), mutable.Stack())

  def fork: Frontend = new Frontend(importCache, pendingSet.clone(), pendingStack.clone())

  implicit private val fe: Frontend = this

  private def guardCircularFuture[T](
      op: PendingFrontendOp,
      loc: Loc
    )(
      f: => Either[FinalResult[T], Future[FinalResult[T]]]
    ): Either[FinalResult[T], Future[FinalResult[T]]] = {
    if (pendingSet contains op) {
      Left {
        Failure {
          def msg(op: PendingFrontendOp): String = op match {
            case PendingTypeOf(symbol)   => s"type of symbol '${symbol.name}'"
            case PendingEvaluate(symbol) => s"value of symbol '${symbol.name}'"
            case PendingSpecialzie(symbol, params) =>
              val paramString = params map {
                case ArgP(expr)       => expr.toSource
                case ArgN(name, expr) => s"$name = ${expr.toSource}"
                case ArgD(base, idxs, expr) =>
                  s"$base#[${idxs.map(_.toSource) mkString ","}] = ${expr.toSource}"
              } mkString ", "
              s"specialization of symbol '${symbol.name}' with parameters: $paramString"
            case PendingImport(path) => s"""imported file "$path""""
          }

          val (cycle, rest) = pendingStack.toList.span(_._1 != op)
          val error = Error(rest.head._2, s"Circular definition: ${msg(op)}")

          val trimmedCycle = cycle.filterNot {
            _._1 pipe {
              case PendingTypeOf(symbol)        => Some(symbol)
              case PendingEvaluate(symbol)      => Some(symbol)
              case PendingSpecialzie(symbol, _) => Some(symbol)
              case _: PendingImport             => None
            } exists {
              _.desc match {
                case _: DescGenIf | //
                    _: DescGenFor | //
                    _: DescGenRange | //
                    _: DescGenScope =>
                  true
                case Desc(Sym(symbol)) => symbol.name.startsWith("`")
                case _                 => false
              }
            }
          }

          error withNotes {
            trimmedCycle.reverse.map { case (op, loc) => Note(loc, s"Depends on ${msg(op)}") }
          } withNote Note(loc, s"Depends on ${msg(op)}")
        }
      }
    } else {
      pendingSet.add(op)
      pendingStack.push((op, loc))
      f tap { _ =>
        pendingStack.pop()
        pendingSet.remove(op)
      }
    }
  }

  def guardCircular[T](
      op: PendingFrontendOp,
      loc: Loc
    )(
      f: => FinalResult[T]
    ): FinalResult[T] =
    guardCircularFuture(op, loc)(Left(f)).fold(identity, _ => unreachable)

  private def doImport(loc: Loc, file: File): FinalResult[Symbol] =
    cc.readFile(file) match {
      case Left(error) => Failure(loc, error)
      case Right(content) =>
        file.getName.dropWhile(_ != '.') match {
          case ".alogic" =>
            elaborate(Source(file, content)) flatMap { desc =>
              typeCheck(desc) map { _ => desc.symbol }
            }
          case _ =>
            Failure(
              loc,
              s"Unable to import file ${file.getCanonicalPath}",
              "unknown filename extension"
            )
        }
    }

  def imprt(path: String, loc: Loc): Either[FinalResult[Symbol], Future[FinalResult[Symbol]]] =
    path.trim pipe { path =>
      if (path.isEmpty) {
        Left(Failure(loc, "Empty import path"))
      } else if (path.startsWith("/")) {
        Left(Failure(loc, "Import path cannot start with a leading '/'"))
      } else {
        val absolute = path.head != '.'
        val searchPaths = if (absolute) {
          // Absolute import
          cc.settings.importSearchDirs
        } else {
          // Relative import
          List(Paths.get(loc.trueFileOpt.get).toAbsolutePath.getParent)
        }
        // Find the referenced file
        val sourceFileOpt = searchPaths.iterator
          .flatMap { searchPath =>
            val asIs = searchPath.resolve(path).toFile
            Option.when(asIs.isFile)(asIs) orElse {
              val withSuffix = searchPath.resolve(path + ".alogic").toFile
              Option.when(withSuffix.isFile)(withSuffix)
            }
          }
          .nextOption()
          .map(_.getCanonicalFile)
        //
        sourceFileOpt match {
          case None =>
            // No such file on the filesystem
            Left {
              Failure {
                if (absolute) {
                  Error(loc, s"""Cannot find absolute import target "$path"""") withNotes {
                    searchPaths.map(path => Note(loc, s"Looked in: $path"))
                  }
                } else {
                  Error(loc, s"""Cannot find relative import target "$path"""") withNote
                    Note(
                      loc, {
                        val expected = searchPaths.head.resolve(path).toFile.getCanonicalPath
                        val lb = new ListBuffer[String]
                        lb.addOne(s"Path: $expected")
                        if (!path.endsWith(".alogic")) {
                          lb.addOne(s"or: $expected.alogic")
                        }
                        lb.addOne("does not exist or is not a regular file")
                        lb.toList
                      }
                    )
                }
              }
            }
          case Some(file) =>
            // Found file. Go ahead and import it
            guardCircularFuture(PendingImport(file.getPath), loc) {
              if (cc.settings.parallel) {
                val frontend = fork
                lazy val theFuture = Future(frontend.doImport(loc, file))
                importCache.putIfAbsent(file.getPath, () => Right(theFuture)) match {
                  case Some(f) => f()
                  case None    => Right(theFuture)
                }
              } else {
                lazy val theResult = this.doImport(loc, file)
                importCache.putIfAbsent(file.getPath, () => Left(theResult)) match {
                  case Some(f) => f()
                  case None    => Left(theResult)
                }
              }
            }
        }
      }
    }

  def typeCheck(tree: Tree): FinalResult[Type] =
    TypeChecker(tree)

  def typeOf(symbol: Symbol, loc: Loc, refresh: Boolean = false): FinalResult[Type] =
    guardCircular(PendingTypeOf(symbol), loc) {
      TypeOf(symbol, refresh)
    }

  def tryEvaluate(expr: Expr): FinalResult[Either[Seq[Note], BigInt]] =
    Evaluate(expr)

  def tryEvaluate(
      symbol: Symbol,
      loc: Loc,
      markUsed: Boolean = true,
      paramCheck: Boolean = false
    ): FinalResult[Either[Seq[Note], Expr]] =
    guardCircular(PendingEvaluate(symbol), loc) {
      Evaluate(symbol, markUsed, paramCheck)
    }

  def evaluate(expr: Expr, hint: => String): FinalResult[BigInt] =
    tryEvaluate(expr).flatMap {
      case Left(notes) =>
        Failure(Error(expr, s"${hint.capitalize} must be a compile time constant") withNotes notes)
      case Right(value) => Complete(value)
    }

  def evaluate(
      symbol: Symbol,
      loc: Loc,
      hint: => String,
      markUsed: Boolean = true,
      paramCheck: Boolean = false
    ): FinalResult[Expr] =
    tryEvaluate(symbol, loc, markUsed, paramCheck).flatMap {
      case Left(notes) =>
        Failure(Error(loc, s"${hint.capitalize} must be a compile time constant") withNotes notes)
      case Right(value) => Complete(value)
    }

  def nameFor(base: String, idxs: List[Expr]): FinalResult[String] =
    if (idxs.isEmpty) {
      Complete(base)
    } else {
      idxs
        .map(evaluate(_, "Identifier index"))
        .distil
        .map(_.mkString(base + "#[", ",", "]"))
    }

  def specialize(symbol: Symbol, params: List[Arg], loc: Loc): FinalResult[Symbol] =
    Specialize(symbol, params, loc)

  def elaborate[T <: Tree: ListElaborable](
      trees: List[T],
      enclosingSymbol: Option[Symbol],
      symtab: SymbolTable,
      paramsOpt: Option[(Loc, List[Arg])]
    ): Result[List[T]] =
    Elaborate.list[T](trees, enclosingSymbol, symtab, paramsOpt)

  def elaborate(
      source: Source
    )(
      implicit
      cc: CompilerContext
    ): FinalResult[Desc] = {
    // Parse the file
    val mb = new MessageBuffer
    Parser[DescPackage](source, SourceContext.Package, mb) pipe {
      case Some(desc) =>
        mb.messages foreach cc.addMessage
        Complete(desc)
      case None => Failure(mb.messages)
    } flatMap { desc => // Run SyntaxCheck
      val messages = SyntaxCheck(desc)
      if (messages.exists(_.isInstanceOf[Error])) {
        Failure(messages)
      } else {
        messages foreach cc.addMessage
        Complete(desc)
      }
    } map { desc => // Normalize some constructs for easier elaboration
      SyntaxNormalize(desc)
    } flatMap {
      case desc: DescPackage =>
        // Elaborate the package definition
        @tailrec
        def loop(desc: Desc): FinalResult[Desc] = Elaborate(desc, Builtins.symbolTable) match {
          case result: FinalResult[Desc] => result
          case Finished(_)               => unreachable
          case Partial(d, _)             => loop(d)
        }
        loop(desc)
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        // Parametrized package, will definitely not need to iterate just yet
        Elaborate(desc, Builtins.symbolTable) match {
          case result: FinalResult[Desc] => result
          case Finished(_)               => unreachable
          case Partial(_, _)             => unreachable
        }
      case _ => unreachable
    }
  }

  def apply(
      source: Source,
      loc: Loc,
      params: List[Arg]
    )(
      implicit
      cc: CompilerContext
    ): Option[(DescPackage, ParOrSeqIterable[DescPackage])] =
    Complete(source) flatMap { source => // Elaborate
      elaborate(source)
    } flatMap { desc => // Type check
      typeCheck(desc) map { _ => desc }
    } flatMap { // Specialize if parametrized
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        elaborate(params, None, Builtins.symbolTable, None)
          .pipe {
            case result: FinalResult[List[Arg]] => result
            case Finished(result)               => Complete(result)
            case Partial(_, rs)                 => Unknown(rs)
          }
          .flatMap { params =>
            specialize(desc.symbol, params, loc)
          }
          .map {
            _.desc.asInstanceOf[DescPackage]
          }
      case desc: DescPackage =>
        if (params.isEmpty) {
          Complete(desc)
        } else {
          val msg = s"Package defined in input file '${source.path}' does not take any parameters."
          Failure(params.map(Error(_, msg)))
        }
      case _ => unreachable
    } flatMap { desc =>
      // Apply Finalize
      val finalized @ (input, dependencies) = Finalize(desc)
      //
      val iterable = (dependencies + input).asPar
      // Apply a secondary SyntaxCheck to ensure 'gen' yielded well formed
      // trees. Also apply UnusedCheck
      val messages = iterable.flatMap(SyntaxCheck.apply) ++ iterable.flatMap(UnusedCheck.apply)
      //
      if (messages.exists(_.isInstanceOf[Error])) {
        Failure(messages.iterator.toSeq)
      } else {
        messages.iterator.toSeq.sortBy(_.loc) foreach cc.addMessage
        Complete(finalized)
      }
    } match {
      case Complete(result) => Some(result)
      case Failure(ms) =>
        ms foreach cc.addMessage
        None
      case Unknown(rs) =>
        val messages = rs.map(_.toMessage)
        // If there is a user error, ignore all internal errors, as they might
        // have been caused by the presence of the user error
        if (!messages.forall(_.isInstanceOf[Ice])) {
          messages.filterNot(_.isInstanceOf[Ice]) foreach cc.addMessage
        } else {
          messages foreach cc.addMessage
        }
        None
    }

}
