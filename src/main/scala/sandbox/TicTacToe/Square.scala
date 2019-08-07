package sandbox.TicTacToe

import sandbox.typeClasses.{Decoder, Encoder}
import sandbox.typeClasses.Encoder._

sealed trait Square {
  def asSquare: Square = this
}

object X extends Square
object O extends Square

object Square {
  val empty = '.'
  val x = 'X'
  val o = 'O'

  def toSymbol(value: Square): Char = value match {
    case X => x
    case O => o
  }

  def opposite(value: Square): Square = value match {
    case X => O
    case O => X
  }

  implicit val squareEncoder: Encoder[Square] = (value: Square) => toSymbol(value).toString

  implicit val squareDecoder: Decoder[Square] = (value: String) => value.headOption.flatMap {
    case Square.x => Some(X)
    case Square.o => Some(O)
    case _ => None
  }

  implicit val squareOptEncoder: Encoder[Option[Square]] = (t: Option[Square]) => t.map(_.encode).getOrElse(empty.toString)

  implicit val squareOptDecoder: Decoder[Option[Square]] = (value: String) => {
    value.headOption.flatMap {
      case Square.x | Square.o | Square.empty => Some(Decoder[Square].decode(value))
      case _ => None
    }
  }
}
