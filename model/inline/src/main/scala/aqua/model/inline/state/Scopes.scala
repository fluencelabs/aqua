package aqua.model.inline.state

import aqua.model.{FuncArrow, LiteralModel, ValueModel, VarModel}
import cats.data.State

case class ScopeLinks(
  name: String,
  variables: Map[String, String],
  arrows: Map[String, String],
  literals: Map[String, LiteralModel]
)

case class ScopeState(
  links: Map[String, ScopeLinks] = Map.empty,
  exports: Map[String, ValueModel] = Map.empty,
  arrows: Map[String, FuncArrow] = Map.empty
)

trait Scopes[S] extends Scoped[S] {
  self =>

  def save(
    name: String,
    variables: Map[String, String],
    arrows: Map[String, String],
    literals: Map[String, LiteralModel]
  ): State[S, Unit]
  def getArrow(scopeName: String, arrow: String): State[S, Option[FuncArrow]]
  def getValue(scopeName: String, value: String): State[S, Option[ValueModel]]
  def scopes: State[S, Map[String, ScopeLinks]]

  def transformS[R](f: R => S, g: (R, S) => R): Scopes[R] = new Scopes[R] {

    override def save(
      name: String,
      variables: Map[String, String],
      arrows: Map[String, String],
      literals: Map[String, LiteralModel]
    ): State[R, Unit] =
      self.save(name, variables, arrows, literals).transformS(f, g)

    override def getArrow(scopeName: String, arrow: String): State[R, Option[FuncArrow]] =
      self.getArrow(scopeName, arrow).transformS(f, g)

    override def getValue(scopeName: String, value: String): State[R, Option[ValueModel]] =
      self.getValue(scopeName, value).transformS(f, g)

    override val scopes: State[R, Map[String, ScopeLinks]] = self.scopes.transformS(f, g)

    override val purge: State[R, R] =
      self.purgeR(f, g)

    override protected def fill(s: R): State[R, Unit] =
      self.fillR(s, f, g)
  }

}

object Scopes {
  def apply[S](implicit scopes: Scopes[S]): Scopes[S] = scopes

  // Default implementation with the most straightforward state – just a Map
  object Simple extends Scopes[ScopeState] {

    override def save(
      name: String,
      variables: Map[String, String],
      arrows: Map[String, String],
      literals: Map[String, LiteralModel]
    ): State[ScopeState, Unit] =
      State.modify(s =>
        s.copy(links = s.links.updated(name, ScopeLinks(name, variables, arrows, literals)))
      )

    override def getArrow(scopeName: String, arrow: String): State[ScopeState, Option[FuncArrow]] =
      for {
        scopes <- State.get
      } yield scopes.links
        .get(scopeName)
        .flatMap(links => links.arrows.get(arrow))
        .flatMap(arrowName => scopes.arrows.get(arrowName))

    override def getValue(scopeName: String, value: String): State[ScopeState, Option[ValueModel]] =
      State.get.map { scopes =>
        println("scopes: " + scopes.links)
        println("scope arrows: " + scopes.arrows.keys)
        println("scope values: " + scopes.exports.keys)
        println(s"try to find $scopeName with $value")
        scopes.links
          .get(scopeName)
          .flatMap { links =>
            println("found links")
            links.literals
              .get(value)
              .orElse(
                links.variables
                  .get(value)
                  .flatMap(varName => scopes.exports.get(varName))
                  .orElse(
                    // we can get arrow as value to pass it like value
                    links.arrows
                      .get(value)
                      .flatMap(arrow =>
                        scopes.arrows.get(arrow).map { funcArrow =>
                          println("generate var model for: " + funcArrow.funcName)
                          VarModel(funcArrow.funcName, funcArrow.arrowType)
                        }
                      )
                  )
              )
          }
      }

    override def scopes: State[ScopeState, Map[String, ScopeLinks]] = State.get.map(_.links)

    override val purge: State[ScopeState, ScopeState] =
      for {
        s <- State.get
        _ <- State.set(ScopeState())
      } yield s

    override protected def fill(s: ScopeState): State[ScopeState, Unit] =
      State.set(s)
  }
}
