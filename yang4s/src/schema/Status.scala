package yang4s.schema

enum Status(val literal: String):
  case Current extends Status("current")
  case Deprecated extends Status("deprecated")
  case Obsolete extends Status("Obsolete")
 

object Status:
  def fromLiteral(literal: String): Option[Status] =
    Status.values.find(_.literal == literal)
  

