package sandbox.TicTacToe

import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}
import sandbox.TicTacToe.typeClasses.Encoder._

sealed trait Square {
  val symbol: Char

  def asSquare: Square = this
}

object X extends Square {
  override val symbol: Char = 'X'
}
object O extends Square {
  override val symbol: Char = 'O'
}

object Square {
  val empty = '.'

  def opposite(value: Square): Square = value match {
    case X => O
    case O => X
  }

  implicit val showSquare: Encoder[Square] = (value: Square) => value.symbol.toString

  implicit val readSquare: Decoder[Square] = (value: String) => value.headOption.flatMap {
    case X.symbol => Some(X)
    case O.symbol => Some(O)
    case _ => None
  }

  implicit val squareOptEncoder: Encoder[Option[Square]] = (t: Option[Square]) => t.map(_.encode).getOrElse(empty.toString)

  implicit val squareOptDecoder: Decoder[Option[Square]] = (value: String) => {
    value.headOption.flatMap {
      case X.symbol | O.symbol | Square.empty => Some(Decoder[Square].decode(value))
      case _ => None
    }
  }
}
