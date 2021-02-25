import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Test1 extends AnyFlatSpec with Matchers {
  "test" should "run" in {

    import cats.parse.{Parser0 ⇒ P0, Parser => P, Numbers}
    /*

def getTime(peer: PeerId, reply: i64 -> string) -> string =
  on peer:
    t <- Peer.timestamp()
  reply(t)

Scope(Var("peer"), List(
  Fetch(
    Var("t"),
    AbilityCall(
      CustomType("Peer"),
      FnName("timestamp"),
      Nil
    )
  )
)),
Unscoped(
  FuncCall("reply", List(Var("t"))
)

(seq
  (call %init_peer_id% ("load" "peer") [] peer)
  (seq
    (call peer ("peer" "timestamp") [] t)
    (call %init_peer_id% ("callback" "reply") [t]) ;; if used in fetch, then put return value
  )
)
;; register peer value
;; register reply callback

code block:
  free variables
  free functions
  new variables
  scope?

  THEN name, types -- maybe bind free to types


line variants:
func(...)
x <- func(...)
Ability.func(...)
x <- Ability.func(...)

par line
xor line

on peer:
  indented lines*

     */
  }
}
