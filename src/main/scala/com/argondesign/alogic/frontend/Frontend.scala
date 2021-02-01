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
import com.argondesign.alogic.core.Messages.Message
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
      Left(Failure(ms.toSeq))
    } else {
      pendingSet.add(op)
      pendingStack.push((op, loc))
      f tap { _ =>
        pendingStack.pop()
        pendingSet.remove(op)
      }
    }
  }

  private def guardCircular[T](
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
            fe.elaborate(Source(file, content)) pipe {
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
        //
        sourceFileOpt match {
          case None =>
            // No such file on the filesystem
            Left {
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
            }
          case Some(file) =>
            // Found file. Go ahead and import it
            guardCircularFuture(PendingImport(file.getCanonicalPath), loc) {
              if (cc.settings.parallel) {
                val frontend = fork
                lazy val theFuture = Future(frontend.doImport(loc, file))
                importCache.putIfAbsent(file.getCanonicalPath, () => Right(theFuture)) match {
                  case Some(f) => f()
                  case None    => Right(theFuture)
                }
              } else {
                lazy val theResult = this.doImport(loc, file)
                importCache.putIfAbsent(file.getCanonicalPath, () => Left(theResult)) match {
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

  def evaluate(expr: Expr, hint: => String): FinalResult[BigInt] =
    Evaluate(expr, hint)

  def evaluate(
      symbol: Symbol,
      loc: Loc,
      hint: => String,
      markUsed: Boolean = true,
      paramCheck: Boolean = false
    ): FinalResult[Expr] =
    guardCircular(PendingEvaluate(symbol), loc) {
      Evaluate(symbol, loc, hint, markUsed, paramCheck)
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
    guardCircular(PendingSpecialzie(symbol, params), loc) {
      Specialize(symbol, params, loc)
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
        def loop(desc: Desc): FinalResult[Desc] = Elaborate(desc, Builtins.symbolTable) match {
          case Finished(_)            => unreachable
          case Partial(d, _)          => loop(d)
          case complete @ Complete(_) => complete
          case unknown: Unknown       => unknown
          case failure: Failure       => failure
        }
        loop(desc).toEither
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        // Parametrized package, will definitely not need to iterate just yet
        Elaborate(desc, Builtins.symbolTable) match {
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
    ): Option[(DescPackage, ParOrSeqIterable[DescPackage])] = {
    // Apply Finalize
    val finalized @ (input, dependencies) = Finalize(desc)
    //
    val iterable = (dependencies + input).asPar
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
    ): Option[(DescPackage, ParOrSeqIterable[DescPackage])] =
    elaborate(source) flatMap { desc =>
      typeCheck(desc).toEither map { _ => desc }
    } pipe {
      case Left(ms)    => ms foreach cc.addMessage; None
      case Right(desc) => Some(desc)
    } flatMap {
      case desc @ DescParametrized(_, _, _: DescPackage, _) =>
        elaborate(params, Builtins.symbolTable, None)
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
          val msg =
            s"Package defined in input file '${source.path}' does not take any parameters."
          params foreach {
            cc.error(_, msg)
          }
          None
        }
      case _ => unreachable
    } flatMap finalize

}
