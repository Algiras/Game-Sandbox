package sandbox.TicTacToe

import cats.Applicative
import cats.instances.option._
import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}
import sandbox.TicTacToe.typeClasses.Encoder._

package object coordinates {
  val combinations: List[Coordinates] = for {
    column <- List(Col1, Col2, Col3)
    row <- List(Row1, Row2, Row3)
  } yield Coordinates(column, row)

  implicit val coordinatesEncoder: Encoder[Coordinates] = (t: Coordinates) => t.column.encode + t.row.encode

  implicit val coordinatesDecoder: Decoder[Coordinates] = (value: String) => if (value.length == 2) {
    Applicative[Option].map2(
      Decoder[ColumnCoordinate].decode(value.charAt(0).toString),
      Decoder[RowCoordinate].decode(value.charAt(1).toString)
    )(Coordinates)
  } else {
    None
  }
}
