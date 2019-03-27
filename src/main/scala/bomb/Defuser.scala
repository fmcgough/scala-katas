package bomb

import enumeratum._
import scala.annotation.tailrec

sealed trait Wire extends EnumEntry
object Wire extends Enum[Wire] {
  val values = findValues

  case object White extends Wire
  case object Black extends Wire
  case object Red extends Wire
  case object Green extends Wire
  case object Orange extends Wire
  case object Purple extends Wire

  def allExcept(excluded: Wire*): Set[Wire] = values.toSet.diff(excluded.toSet)
}

object Defuser extends App {
  import Wire._


  def nextWireCandidates(wire: Wire): Set[Wire] = wire match {
    case White => allExcept(White, Black)

    case Red => Set(Green)

    case Black => allExcept(White, Green, Orange)

    case Orange => Set(Red, Black)

    case Green => Set(Orange, White)

    case Purple => allExcept(Purple, Green, Orange, White)
 }

  def defuse(wireColours: Seq[String]): String = {
    val wires = wireColours.map(Wire.withNameLowercaseOnly).toList

    @tailrec
    def cutNext(wires: List[Wire]): String = {
      wires match {
        case current :: next :: tail if nextWireCandidates(current).contains(next) =>
          cutNext(next :: tail)

        case _ :: Nil | Nil => "Bomb defused"

        case _ => "Boom!"
      }
    }

    cutNext(wires)
  }

  println(defuse(args))
}
