package sandbox.TicTacToe

import cats.instances.option._
import cats.syntax.show._
import cats.{Applicative, Show}
import sandbox.TicTacToe.typeClasses.Read

package object coordinates {
  val A1 = Coordinates(Col1, Row1)
  val A2 = Coordinates(Col1, Row2)
  val A3 = Coordinates(Col1, Row3)
  val B1 = Coordinates(Col2, Row1)
  val B2 = Coordinates(Col2, Row2)
  val B3 = Coordinates(Col2, Row3)
  val C1 = Coordinates(Col3, Row1)
  val C2 = Coordinates(Col3, Row2)
  val C3 = Coordinates(Col3, Row3)

  implicit val showCoordinates = new Show[Coordinates] {
    override def show(t: Coordinates): String = t.column.show + t.row.show
  }

  implicit val readCoordinates = new Read[Coordinates] {
    override def read(value: String): Option[Coordinates] = if (value.length == 2) {
      Applicative[Option].map2(
        Read[ColumnCoordinate].read(value.charAt(0).toString),
        Read[RowCoordinate].read(value.charAt(1).toString)
      )(Coordinates)
    } else {
      None
    }
  }
}
