package sandbox.TicTacToe.coordinates

import cats.Show
import sandbox.TicTacToe.typeClasses.Read

sealed trait ColumnCoordinate

case object Col1 extends ColumnCoordinate
case object Col2 extends ColumnCoordinate
case object Col3 extends ColumnCoordinate

object ColumnCoordinate {
  implicit val showColumnCoordinates = new Show[ColumnCoordinate] {
    override def show(t: ColumnCoordinate): String = t match {
      case Col1 => "A"
      case Col2 => "B"
      case Col3 => "C"
    }
  }


  implicit val readColumnCoordinates = new Read[ColumnCoordinate] {
    override def read(value: String): Option[ColumnCoordinate] = value match {
      case "A" => Some(Col1)
      case "B" => Some(Col2)
      case "C" => Some(Col3)
      case _ => None
    }
  }
}

