package bomb

import org.scalatest._

class DefuserSpec extends WordSpecLike with Matchers {

  "Defuser" should {

    "defuse the example given" in {
      val input = "white" :: "red" :: "green" :: "white" :: Nil
      Defuser.defuse(input) should be ("Bomb defused")
    }

    "blow up on the example given" in {
      val input = "white" :: "orange" :: "green" :: "white" :: Nil
      Defuser.defuse(input) should be ("Boom!")
    }

    "not blow up on an empty sequence of wires" in {
      noException should be thrownBy {
        Defuser.defuse(Nil)
      }
    }
  }
}
