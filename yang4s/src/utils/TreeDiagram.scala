package yang4s.utils

import yang4s.schema.SchemaNode.*
// import yang4s.schema.SchemaModule.*
import yang4s.schema.*
import yang4s.schema.SchemaDefinition.*
import yang4s.schema.SchemaModule.Submodule
import cats.instances.boolean

// https://datatracker.ietf.org/doc/html/rfc8340

object TreeDiagram {
  private def printQName(qName: QName, mod: SchemaModule.Module) = {
    if (qName.namespace == mod.namespace) {
      qName.localName
    } else {
      qName.qualifiedName
    }
  }

  def printRow(
    prefix: String,
    module: SchemaModule.Module,
    meta: SchemaMeta,
    dataDefs: List[DataNode],
    last: Boolean,
    opts: String = "",
    suffix: Option[String] = None
  ): String = {
    val flag = if (meta.config) "rw" else "ro"
    val status = {
      meta.status match
      case Status.Current                  => "+"
      case yang4s.schema.Status.Deprecated => "x"
      case Status.Obsolete                 => "o"
    }

    prefix ++ s"$status--$flag ${meta.qName.localName}$opts${suffix.map(s => s" $s").getOrElse("")}\n" ++ printDataDefs(
      dataDefs,
      module,
      prefix ++ { if (last) "   " else "|  " }
    )
  }

  private def printTerminalRow(
    prefix: String,
    module: SchemaModule.Module,
    node: SchemaNode.Leaf | SchemaNode.LeafList,
    last: Boolean,
    colLength: Int
  ): String =
    val (meta, tpe, indicator) = node match
      case Leaf(meta, tpe, mandatory) =>
        val indicator = if mandatory then "" else "?"
        (meta, tpe, indicator)

      case LeafList(meta, tpe) =>
        (meta, tpe, "*")

    val features =
      meta.ifFeatures match
        case Nil => ""
        case fs  => fs.map(printQName(_, module)).mkString("{", " ", "}?")

    val gap = colLength - meta.qName.localName.length

    val typeName =
      val qName = tpe.meta.qName
      if BuiltInType.isBuiltin(qName.localName) then qName.localName
      else printQName(qName, module)

    printRow(
      prefix,
      module,
      meta,
      List.empty,
      last,
      opts = indicator,
      suffix = Some(s"${" ".repeat(gap + (4 - indicator.length))}$typeName $features")
    )

  private def printDataDef(
      node: DataNode,
      mod: SchemaModule.Module,
      isLast: Boolean,
      prefix: String,
      colLength: Int
  ): String = {

    node match
      case Container(meta, children) => printRow(prefix, mod, meta, children, isLast)
      case ListNode(meta, key, children) => printRow(prefix, mod, meta, children, isLast, opts = "*", suffix = Some(s"[${key.localName}]"))
      case leaf : Leaf  => printTerminalRow(prefix, mod, leaf, isLast, colLength)
      case leafList : LeafList => printTerminalRow(prefix, mod, leafList, isLast, colLength)
  }

  private def printDataDefs(
      nodes: List[DataNode],
      mod: SchemaModule.Module,
      prefix: String = ""
  ): String = {
    val longestName = nodes
      .map(_.meta.qName.localName)
      .reduceOption { (a, b) =>
        if (a.length >= b.length) a else b
      }
      .map(_.length)
      .getOrElse(0)
    nodes.zipWithIndex
      .map((d, idx) =>
        val isLast = idx == nodes.length - 1
        printDataDef(d, mod, isLast, prefix, longestName)
      )
      .mkString
  }

  def printModules(modules: SchemaModule*): String = {
    val implementedModules: Seq[SchemaModule.Module] =
      modules.collect {
        case m: SchemaModule.Module if m.isImplemented => m
      }

    implementedModules.map { m =>
      s"module: ${m.name}\n${{ printDataDefs(m.dataDefs, m, "  ") }}"
    }.mkString
  }
}
