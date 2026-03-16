package yang4s.schema

import SchemaDefinition.*

sealed trait SchemaNode {
  def meta: SchemaMeta

  def name = meta.qName.localName
  def namespace = meta.qName.namespace
  def description = meta.description
}

object SchemaNode {
  sealed trait DataNode extends SchemaNode

  final case class Container(
      meta: SchemaMeta,
      children: List[DataNode]
  ) extends DataNode

  final case class ListNode(
      meta: SchemaMeta,
      key: QName,
      children: List[DataNode]
  ) extends DataNode

  final case class Leaf(
      meta: SchemaMeta,
      tpe: TypeDefinition,
      mandatory: Boolean
  ) extends DataNode

  final case class LeafList(
      meta: SchemaMeta,
      tpe: TypeDefinition
  ) extends DataNode

  extension (self: DataNode)
    def hasMandatoryDescendant: Boolean =
      self match
        case Leaf(_, _, mandatory)    => mandatory
        case LeafList(_, _)           => false
        case Container(_, children)   => children.exists(_.hasMandatoryDescendant)
        case ListNode(_, _, children) => children.exists(_.hasMandatoryDescendant)
}
