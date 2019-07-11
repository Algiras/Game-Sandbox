package sandbox.TicTacToe.coordinates

import cats.Show
import sandbox.TicTacToe.typeClasses.Read

sealed trait RowCoordinate

case object Row1 extends RowCoordinate
case object Row2 extends RowCoordinate
case object Row3 extends RowCoordinate

object RowCoordinate {
  implicit val showRowCoordinates = new Show[RowCoordinate] {
    override def show(t: RowCoordinate): String = t match {
      case Row1 => "1"
      case Row2 => "2"
      case Row3 => "3"
    }
  }

  implicit val readRowCoordinates = new Read[RowCoordinate] {
    override def read(value: String): Option[RowCoordinate] = value match {
      case "1" => Some(Row1)
      case "2" => Some(Row2)
      case "3" => Some(Row3)
      case _ => None
    }
  }
}
