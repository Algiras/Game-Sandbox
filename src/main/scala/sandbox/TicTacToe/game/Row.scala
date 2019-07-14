package sandbox.TicTacToe.game

import cats.Applicative
import cats.instances.option._
import sandbox.TicTacToe.Square
import sandbox.TicTacToe.Square._
import sandbox.TicTacToe.typeClasses.Encoder._
import sandbox.TicTacToe.typeClasses.{Decoder, Encoder}

case class Row(col1: Option[Square], col2: Option[Square], col3: Option[Square]) {
  val full: Boolean = col1.isDefined && col2.isDefined && col3.isDefined
}

object Row {
  def apply(col1: Option[Square], col2: Option[Square], col3: Option[Square]): Row = new Row(col1, col2, col3)

  implicit val rowEncoder: Encoder[Row] = (t: Row) => Seq(t.col1, t.col2, t.col3).map(_.encode).mkString(" ")
  implicit val rowDecoder: Decoder[Row] = (value: String) => if (value.length == 5) {
    Applicative[Option].map3(
      Decoder[Option[Square]].decode(value.charAt(0).toString),
      Decoder[Option[Square]].decode(value.charAt(2).toString),
      Decoder[Option[Square]].decode(value.charAt(4).toString)
    )(Row(_, _ , _))
  } else {
    None
  }
}
