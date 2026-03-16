package yang4s.schema.compiler

import cats.*
import cats.data.StateT
import cats.syntax.all.*

import yang4s.schema.*
import yang4s.parser.*

import SchemaCompiler.*

opaque type SchemaCompiler[A] = StateT[SchemaCompiler.Result, SchemaCompiler.CompileCtx, A]

object SchemaCompiler
    extends SchemaCompilerSyntax
    with SchemaCompilerOps
    with SchemaCompilerCatsInstances {
  type Error = String
  type Result[A] = Either[Error, A]

  // Todo: Temporary
  type Prefix = String

  def apply[A](stateT: StateT[Result, CompileCtx, A]): SchemaCompiler[A] = stateT

  private[compiler] def unwrap[A](comp: SchemaCompiler[A]): StateT[Result, CompileCtx, A] = comp

  final case class CompileCtx(
      namespace: Namespace,
      scope: Scope,
      schemaCtx: SchemaContext,
      imports: Map[Prefix, Namespace],
      features: List[FeatureDefinition]
  ) {
    def stmt = scope.stmt
  }

  object CompileCtx {
    extension (self: CompileCtx) {
      def focus(stmt: Statement) = self.copy(scope = self.scope.child(stmt))

      def toError(message: String): Error = {
        s"${self.scope.stmt.keyword}: $message"
      }
      def addTypeDefToScope(td: TypeDefinition): CompileCtx = {
        self.copy(scope = self.scope.copy(typeDefinitions = td :: self.scope.typeDefinitions))
      }

      def resolvePrefix(prefix: Prefix): Option[Namespace] = {
        self.imports
          .get(prefix)
          .orElse(self.namespace.prefix.filter(_ == prefix).map(_ => self.namespace))
      }
    }

    def fromStmt(stmt: Statement, schemaCtx: SchemaContext): CompileCtx =
      CompileCtx(Namespace.DEFAULT, Scope.fromStmt(stmt), schemaCtx, Map.empty, List.empty)
  }

}

trait SchemaCompilerSyntax {
  extension [A](self: SchemaCompiler[A]) {
    def compile(stmt: Statement, schemaCtx: SchemaContext): Either[Error, (CompileCtx, A)] =
      unwrap(self).run(CompileCtx.fromStmt(stmt, schemaCtx))

    def compile(ctx: CompileCtx): Either[Error, (CompileCtx, A)] =
      unwrap(self).run(ctx)

    def compileSubstmt(stmt: Statement): SchemaCompiler[A] =
      withCtx(_.focus(stmt))

    def compileSubstmts(stmts: List[Statement]): SchemaCompiler[List[A]] =
      stmts.map(compileSubstmt(_)).sequence

    def withCtx(f: CompileCtx => CompileCtx): SchemaCompiler[A] =
      SchemaCompiler(StateT { ctx0 =>
        compile(f(ctx0)).map { case (_, a) =>
          (ctx0, a)
        }
      })


    def filter(predicate: Statement => Boolean): SchemaCompiler[List[A]] =
      currentCtx.flatMap(ctx => compileSubstmts(ctx.stmt.substatements.filter(predicate)))

    def toOption: SchemaCompiler[Option[A]] =
      readCtxF(ctx => fromResult(Right(compile(ctx).toOption.map(_._2))))

    def require(kw: Keyword): SchemaCompiler[A] = readCtxF { ctx =>
      ctx.stmt.substatements
        .find(_.keyword == kw.literal)
        .fold(SchemaCompiler.fail("Required statement is missing."))(compileSubstmt(_))
    }

    def optional(kw: Keyword): SchemaCompiler[Option[A]] = currentCtx.flatMap { ctx =>
      require(kw).toOption
    }

    def many(kw: Keyword): SchemaCompiler[List[A]] = filter(_.keyword == kw.literal)
  }
}

trait SchemaCompilerOps { self: SchemaCompilerCatsInstances =>
  def readCtx[A](f: CompileCtx => A): SchemaCompiler[A] =
    SchemaCompiler(StateT.inspect(f))

  def readCtxF[A](f: CompileCtx => SchemaCompiler[A]): SchemaCompiler[A] =
    currentCtx.flatMap(f)

  def currentCtx: SchemaCompiler[CompileCtx] =
    SchemaCompiler(StateT.get)

  def updateCtx(f: CompileCtx => CompileCtx): SchemaCompiler[Unit] =
    SchemaCompiler(StateT.modify(f))

  def fail[A](message: String): SchemaCompiler[A] =
    SchemaCompiler(StateT { ctx =>
      Left(ctx.toError(message))
    })

  def failWith[A](error: Error): SchemaCompiler[A] =
    SchemaCompiler(StateT.liftF(Left(error)))

  def succeed[A](a: A): SchemaCompiler[A] =
    SchemaCompiler(StateT.pure(a))

  def fromResult[A](result: Result[A]): SchemaCompiler[A] =
    SchemaCompiler(StateT.liftF(result))

  def fromEither[A](fa: Either[String, A]): SchemaCompiler[A] =
    SchemaCompiler(StateT { ctx =>
      StateT.liftF(fa.leftMap(ctx.toError(_))).run(ctx)
    })
}

trait SchemaCompilerCatsInstances {
  given Monad[SchemaCompiler] with {
    def pure[A](a: A): SchemaCompiler[A] = SchemaCompiler.succeed(a)

    def flatMap[A, B](fa: SchemaCompiler[A])(f: A => SchemaCompiler[B]): SchemaCompiler[B] =
      SchemaCompiler(unwrap(fa).flatMap(a => unwrap(f(a))))

    def tailRecM[A, B](a: A)(f: A => SchemaCompiler[Either[A, B]]): SchemaCompiler[B] =
      SchemaCompiler(
        summon[Monad[[A] =>> StateT[Result, CompileCtx, A]]].tailRecM(a)(a0 => unwrap(f(a0)))
      )
  }
}
