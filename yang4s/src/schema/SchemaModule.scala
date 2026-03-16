package yang4s.schema

import yang4s.parser.Statement
import yang4s.schema.SchemaNode.*
import yang4s.schema.SchemaDefinition.*


sealed trait SchemaModule {
  def body: SchemaModule.ModuleBody

  def dataDefs: List[DataNode] = body.dataDefs
  def typeDefs: List[TypeDefinition] = body.typeDefs
}

object SchemaModule {
  final case class Module(
    name: String,
    namespace: Namespace,
    body: ModuleBody,
    ) extends SchemaModule

  /*
   * Todo: Add belongs to.
   */
  final case class Submodule(
    name: String,
    body: ModuleBody,
    ) extends SchemaModule


  extension (self: SchemaModule) {
    def isImplemented: Boolean = self match
      case Module(_, _, body) => !self.dataDefs.isEmpty
      case _: Submodule => false
  }

  final case class ModuleBody (
    dataDefs: List[DataNode],
    typeDefs: List[TypeDefinition],
    features: List[FeatureDefinition],
  )
}

