package yang4s.schema

case class SchemaMeta(
    qName: QName,
    description: Option[String],
    config: Boolean,
    status: Status,
    ifFeatures: List[QName],
)
