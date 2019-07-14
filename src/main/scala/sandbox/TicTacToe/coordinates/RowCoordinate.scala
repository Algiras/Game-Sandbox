package sandbox.TicTacToe.coordinates

import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}

sealed trait RowCoordinate

case object Row1 extends RowCoordinate
case object Row2 extends RowCoordinate
case object Row3 extends RowCoordinate

object RowCoordinate {
  implicit val rowCoordinatesEncoder: Encoder[RowCoordinate] = {
    case Row1 => "1"
    case Row2 => "2"
    case Row3 => "3"
  }

  implicit val rowCoordinatesDecoder: Decoder[RowCoordinate] = {
    case "1" => Some(Row1)
    case "2" => Some(Row2)
    case "3" => Some(Row3)
    case _ => None
  }
}
