package sandbox.TicTacToe

import cats.Applicative
import cats.instances.option._
import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}
import sandbox.TicTacToe.typeClasses.Encoder._

package object game {
  val empty = Game(
    Row(None, None, None),
    Row(None, None, None),
    Row(None, None, None)
  )

  implicit val gameDecoder: Decoder[Game] = (value: String) => {
    val lines = value.split('\n')

    if (lines.length == 3) {
      Applicative[Option].map3(
        Decoder[Row].decode(lines(0)),
        Decoder[Row].decode(lines(1)),
        Decoder[Row].decode(lines(2))
      )(Game)
    } else {
      None
    }
  }

  implicit val gameEncoder: Encoder[Game] = (t: Game) => Seq(t.row1, t.row2, t.row3).map(_.encode).mkString("\n")

}
