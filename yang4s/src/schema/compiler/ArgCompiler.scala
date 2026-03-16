package yang4s.schema.compiler

import yang4s.schema.*

import cats.data.StateT
import cats.syntax.all.*
import java.net.URI
import scala.util.Try

trait ArgCompiler {
  def stringArg: SchemaCompiler[String] = SchemaCompiler.readCtxF(
    _.stmt.arg.fold(SchemaCompiler.fail("Expected an arguement."))(SchemaCompiler.succeed(_))
  )

  def boolArg: SchemaCompiler[Boolean] = stringArg.flatMap { arg =>
    val boolOpt: Option[Boolean] = {
      if (arg == "true")
        Some(true)
      else if (arg == "false")
        Some(false)
      else None
    }

    boolOpt.fold(SchemaCompiler.fail("Expected a boolean arguement."))(SchemaCompiler.succeed(_))
  }

  def uriArg: SchemaCompiler[URI] = stringArg.flatMap { arg =>
    Try(URI(arg))
      .fold(_ => SchemaCompiler.fail("Expected an valid URI arguement."), SchemaCompiler.succeed(_))
  }

  def qNameArg: SchemaCompiler[QName] = stringArg.flatMap { arg =>
    val (prefixOption, identifier): (Option[String], String) = arg.split(":", 2) match
      case Array(prefix, identifier) => (Some(prefix), identifier)
      case _                         => (None, arg)

    val namespaceCompiler = SchemaCompiler.currentCtx.flatMap { ctx =>
      prefixOption.fold(Some(ctx.namespace))(ctx.resolvePrefix) match
        case Some(value) => SchemaCompiler.succeed(value)
        case None        => SchemaCompiler.fail(ctx.toError("Not a valid identifier."))
    }

    namespaceCompiler.map(QName(_, identifier))
  }
}
