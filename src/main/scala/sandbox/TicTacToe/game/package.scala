package sandbox.TicTacToe

import cats.{Applicative, Show}
import cats.instances.option._
import cats.syntax.show._
import sandbox.TicTacToe.typeClasses.Read

package object game {
  val empty = Game(
    Row(None, None, None),
    Row(None, None, None),
    Row(None, None, None)
  )

  implicit val readGame: Read[Game] = (value: String) => {
    val lines = value.split('\n')

    if (lines.length == 3) {
      Applicative[Option].map3(
        Read[Row].read(lines(0)),
        Read[Row].read(lines(1)),
        Read[Row].read(lines(2))
      )(Game)
    } else {
      None
    }
  }

  implicit val showGame: Show[Game] = (t: Game) => Seq(t.row1.show, t.row2.show, t.row3.show).mkString("\n")

}
