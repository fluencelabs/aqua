package aqua.lsp

import aqua.types.*

import cats.Show
import cats.syntax.show.*

object TypeShow {
  given Show[DataType] = {
    case LiteralType.signed =>
      "i32"
    case LiteralType.unsigned =>
      "u32"
    case LiteralType.number =>
      "u32"
    case LiteralType.float =>
      "f32"
    case LiteralType.string =>
      "string"
    case LiteralType.bool =>
      "bool"
    case t =>
      t.toString
  }

  given Show[Type] = {
    case ArrayType(el) =>
      s"[]${el.show}"
    case OptionType(el) =>
      s"?${el.show}"
    case StreamType(el) =>
      s"*${el.show}"
    case ArrowType(domain, codomain) =>
      val domainStr = domain match {
        case _: LabeledConsType =>
          domain.toLabelledList().map { case (s, t) => s"$s: ${t.show}" }.mkString("(", ", ", ")")
        case _ => domain.toList.mkString("(", ", ", ")")
      }
      val codomainStr = codomain.toList match {
        case Nil => ""
        case l => " -> " + l.mkString(", ")
      }
      domainStr + codomainStr
    case nt: NamedType =>
      s"${nt.fullName}(${nt.fields.map(_.show).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")})"
    case t: DataType =>
      t.show
    case t =>
      t.toString
  }
}
