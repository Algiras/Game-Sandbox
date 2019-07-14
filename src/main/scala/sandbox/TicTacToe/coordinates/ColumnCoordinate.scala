package sandbox.TicTacToe.coordinates

import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}

sealed trait ColumnCoordinate

case object Col1 extends ColumnCoordinate
case object Col2 extends ColumnCoordinate
case object Col3 extends ColumnCoordinate

object ColumnCoordinate {
  implicit val columnCoordinatesEncoder: Encoder[ColumnCoordinate] = {
    case Col1 => "A"
    case Col2 => "B"
    case Col3 => "C"
  }

  implicit val columnCoordinatesDecoder: Decoder[ColumnCoordinate] = {
    case "A" => Some(Col1)
    case "B" => Some(Col2)
    case "C" => Some(Col3)
    case _ => None
  }
}

