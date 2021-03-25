package aqua.model

import aqua.semantics.Type

sealed trait OpTag

case object SeqTag extends OpTag
case object ParTag extends OpTag
case object XorTag extends OpTag
case class OnTag(peerId: ValueModel) extends OpTag
case class NextTag(item: String) extends OpTag
case class MatchMismatchTag(left: ValueModel, right: ValueModel, shouldMatch: Boolean) extends OpTag
case class ForTag(item: String, iterable: ValueModel) extends OpTag

case class CoalgebraTag(
  ability: Option[AbilityModel],
  funcName: String,
  args: List[(ValueModel, Type)],
  exportTo: Option[String]
) extends OpTag
