package sandbox.TicTacToe

import cats.Show
import sandbox.TicTacToe.typeClasses.Read

sealed trait Square

object X extends Square
object O extends Square

object Square {

  def opposite(value: Square): Square = value match {
    case X => O
    case O => X
  }

  implicit val showSquare: Show[Square] = {
    case X => "X"
    case O => "O"
  }

  implicit val readSquare: Read[Square] = (value: String) => value.headOption.flatMap {
    case 'X' => Some(X)
    case 'O' => Some(O)
    case _ => None
  }


}
