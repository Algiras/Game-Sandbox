package sandbox.TicTacToe

import cats.Applicative
import cats.instances.option._
import sandbox.typeClasses.{Decoder, Encoder}
import sandbox.typeClasses.Encoder._

package object coordinates {
  val combinations: List[Coordinate] = for {
    column <- List(Col1, Col2, Col3)
    row <- List(Row1, Row2, Row3)
  } yield Coordinate(column, row)

  implicit val coordinatesEncoder: Encoder[Coordinate] = (t: Coordinate) => t.column.encode + t.row.encode

  implicit val coordinatesDecoder: Decoder[Coordinate] = (value: String) => if (value.length == 2) {
    Applicative[Option].map2(
      Decoder[ColumnCoordinate].decode(value.charAt(0).toString),
      Decoder[RowCoordinate].decode(value.charAt(1).toString)
    )(Coordinate)
  } else {
    None
  }
}
