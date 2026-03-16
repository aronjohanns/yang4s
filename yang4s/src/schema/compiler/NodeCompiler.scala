package yang4s.schema.compiler

import yang4s.schema.*
import yang4s.schema.SchemaNode.*
import yang4s.schema.compiler.args.*
import cats.syntax.all.*

import SchemaCompiler.*
import yang4s.parser.Statement

object NodeCompiler {
  case class Import(prefix: String, moduleName: String)

  def moduleCompiler: SchemaCompiler[SchemaModule.Module] = for {
    moduelName <- stringArg
    prefix <- prefixCompiler.require(Keyword.Prefix)
    namespace <- namespaceCompiler
      .require(Keyword.Namespace)
      .flatTap(ns => updateCtx(_.copy(namespace = ns.copy(prefix = Some(prefix)))))
    imports <- importCompiler
      .many(Keyword.Import)
      .flatTap(resolveImports)
    featureDefs <- featureDefinitionCompiler
      .many(Keyword.Feature)
      .flatTap(fds => updateCtx(ctx => ctx.copy(features = fds)))
    typeDefs <- resolveTypeDefinitions
    dataDefs <- dataNodeCompiler.filter(isDataNodeStatement)
  } yield (SchemaModule.Module(
    moduelName,
    namespace,
    SchemaModule.ModuleBody(dataDefs, typeDefs, featureDefs)
  ))

  // Module header
  def importCompiler: SchemaCompiler[Import] = for {
    moduleName <- stringArg
    prefix <- prefixCompiler.require(Keyword.Prefix)
  } yield (Import(prefix, moduleName))

  def resolveImports(imports: List[Import]): SchemaCompiler[Unit] =
    for {
      ctx0 <- currentCtx
      (schemaCtx, modules) <- fromEither(
        ctx0.schemaCtx.loadModules(imports.map(i => ModuleName(i.moduleName, None)))
      )
      _ <- updateCtx(
        _.copy(
          imports = imports
            .map(_.prefix)
            .zip(modules)
            .map((p, m) => (p, m.namespace.copy(prefix = Some(p))))
            .toMap,
          scope = ctx0.scope
            .copy(typeDefinitions = ctx0.scope.typeDefinitions ++ modules.flatMap(_.typeDefs)),
          schemaCtx = schemaCtx
        )
      )
    } yield ()

  def featureDefinitionCompiler: SchemaCompiler[FeatureDefinition] =
    metaCompiler.map(FeatureDefinition(_))

  // Schema Meta Compilers

  def metaCompiler: SchemaCompiler[SchemaMeta] = for {
    qName <- qNameArg
    status <- statusCompiler.optional(Keyword.Status)
    ifFeatures <- ifFeatureCompiler.many(Keyword.IfFeature)
  } yield (SchemaMeta(qName, None, false, status.getOrElse(Status.Current), ifFeatures))

  def dataMetaCompiler: SchemaCompiler[SchemaMeta] = for {
    config0 <- readCtx(_.scope.config)
    meta <- metaCompiler
    config1 <- configCompiler
      .optional(Keyword.Config)
      .map(_.getOrElse(config0))
      .map(_ && config0)
      .flatTap(c => updateCtx(ctx => ctx.copy(scope = ctx.scope.copy(config = c))))
  } yield (meta.copy(config = config1))

  // Types

  /** Builds the type definition from statement
    */
  def typeDefinitionCompiler: SchemaCompiler[TypeDefinition] =
    for {
      meta <- metaCompiler
      tpe <- resolveType.require(Keyword.Type)
    } yield (TypeDefinition(meta, tpe.builtIn))

  def resolveTypeDefFromScope(qName: QName, scope: Scope): SchemaCompiler[TypeDefinition] =
    readCtxF { ctx =>
      val runningCtx = ctx.copy(scope = scope)

      val typeDefStmts =
        scope.stmt.substatements
          .filter(_.keyword == Keyword.TypeDef.literal)

      val qNamesCompiler =
        typeDefStmts
          .map(qNameArg.compileSubstmt(_))
          .sequence

      qNamesCompiler
        .map { qNames =>
          qNames
            .zip(typeDefStmts)
            .find(_._1 == qName)
            .map(_._2)
        }
        .flatMap { stmt =>
          stmt.fold(fail(s"Unknown type. ${qName}"))(
            typeDefinitionCompiler.compileSubstmt(_)
          )
        }
        .withCtx(_ => runningCtx)
    }

  /** Resolves type from ctx or parent sibling statements if not yet resolved
    */
  def resolveTypeDefinition(qName: QName, scope: Scope): SchemaCompiler[TypeDefinition] = readCtxF {
    ctx =>
      ctx.scope.typeDefinitions
        .find(_.meta.qName == qName)
        .fold(
          resolveTypeDefFromScope(qName, scope).flatTap(td => updateCtx(_.addTypeDefToScope(td)))
        )(succeed(_))
  }

  def resolveTypeDefinitions: SchemaCompiler[List[TypeDefinition]] = {
    for {
      scope <- readCtx(_.scope)
      typeDefs <- qNameArg.flatMap(resolveTypeDefinition(_, scope)).many(Keyword.TypeDef)
      _ <- updateCtx(ctx =>
        ctx.copy(scope = ctx.scope.copy(typeDefinitions = typeDefs ++ ctx.scope.typeDefinitions))
      )
    } yield (typeDefs)
  }

  /** Resolve the type definition from type statement
    */
  def resolveType: SchemaCompiler[TypeDefinition] = qNameArg.flatMap { qName =>
    readCtx(_.scope).flatMap { scope =>
      BuiltInType
        .fromLiteral(qName.localName)
        .fold(resolveTypeDefinition(qName, scope.parent.flatMap(_.parent).getOrElse(scope)))(b =>
          succeed(TypeDefinition.fromBuiltIn(b))
        )
    }
  }

  // Data defining

  def containerCompiler: SchemaCompiler[DataNode] =
    for {
      meta <- dataMetaCompiler
      dataDefs <- dataNodeCompiler.filter(isDataNodeStatement)
    } yield (Container(meta, dataDefs))

  def listCompiler: SchemaCompiler[DataNode] =
    for {
      meta <- dataMetaCompiler
      key <- keyCompiler.require(Keyword.Key)
      dataDefs <- dataNodeCompiler.filter(isDataNodeStatement).flatMap(validateListKey(_, key))
    } yield (ListNode(meta, key, dataDefs))

  def validateListKey(dataDefs: List[DataNode], key: QName): SchemaCompiler[List[DataNode]] = {
    val (before, after) = dataDefs.span(_.meta.qName == qNameArg)

    after match
      case (leaf: Leaf) :: tail =>
        succeed(before ::: leaf.copy(mandatory = true) :: tail)
      case _ => fail("Not a valid key.")
  }

  def leafCompiler: SchemaCompiler[DataNode] =
    for {
      meta <- dataMetaCompiler
      tpe <- resolveType.require(Keyword.Type)
      mandatory <- mandatoryCompiler.optional(Keyword.Mandatory)
    } yield (Leaf(meta, tpe, mandatory.getOrElse(false)))

  def leafListCompiler: SchemaCompiler[DataNode] =
    for {
      meta <- dataMetaCompiler
      tpe <- resolveType.require(Keyword.Type)
    } yield (LeafList(meta, tpe))

  val dataNodeCompilers: Map[Keyword, SchemaCompiler[DataNode]] = Map(
    Keyword.Container -> containerCompiler,
    Keyword.List -> listCompiler,
    Keyword.Leaf -> leafCompiler,
    Keyword.LeafList -> leafListCompiler
  )

  def isDataNodeStatement(stmt: Statement): Boolean =
    Keyword.fromLiteral(stmt.keyword).filter(dataNodeCompilers.contains(_)).fold(false)(_ => true)

  def dataNodeCompiler: SchemaCompiler[DataNode] = readCtxF { ctx =>
    Keyword.fromLiteral(ctx.stmt.keyword).flatMap(dataNodeCompilers.lift).getOrElse(fail("Unexpected error"))
  }
  // Misc

  def keyCompiler: SchemaCompiler[QName] = qNameArg

  def prefixCompiler: SchemaCompiler[Prefix] = stringArg

  def namespaceCompiler: SchemaCompiler[Namespace] = uriArg.map(Namespace(_, None))

  def mandatoryCompiler: SchemaCompiler[Boolean] = boolArg

  def ifFeatureCompiler: SchemaCompiler[QName] = for {
    features <- readCtx(_.features)
    qName <- qNameArg
    _ <- features
      .find(_.meta.qName == qName)
      .fold(fail("Feature does not exists."))(_ => succeed(()))
  } yield (qName)

  // Common
  def statusCompiler: SchemaCompiler[Status] = for {
    arg <- stringArg
    status <- fromEither(Status.fromLiteral(arg).toRight("Not a valid status."))
  } yield (status)

  def configCompiler: SchemaCompiler[Boolean] = boolArg
}
