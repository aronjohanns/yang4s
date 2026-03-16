package yang4s.schema

sealed trait SchemaDefinition {
  def meta: SchemaMeta
}

object SchemaDefinition {
  final case class TypeDefinition(
      meta: SchemaMeta,
      builtIn: BuiltInType
  ) extends SchemaDefinition

  object TypeDefinition {
    def fromBuiltIn(builtIn: BuiltInType): TypeDefinition =
      TypeDefinition(
        SchemaMeta(
          QName.defaultNamespace(builtIn.literal),
          None,
          config = false,
          status = Status.Current,
          ifFeatures = Nil
        ),
        builtIn
      )
  }

  final case class FeatureDefinition(meta: SchemaMeta) extends SchemaDefinition
}
