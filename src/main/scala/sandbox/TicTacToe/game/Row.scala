package sandbox.TicTacToe.game

import cats.{Applicative, Show}
import sandbox.TicTacToe.Square
import cats.syntax.show._
import cats.instances.option._
import sandbox.TicTacToe.typeClasses.Read

case class Row(col1: Option[Square], col2: Option[Square], col3: Option[Square]) {
  val full: Boolean = col1.isDefined && col2.isDefined && col3.isDefined
}

object Row {
  def apply(col1: Option[Square], col2: Option[Square], col3: Option[Square]): Row = new Row(col1, col2, col3)

  implicit val showSquareOpt: Show[Option[Square]] = (t: Option[Square]) => t.map(_.show).getOrElse(".")

  implicit val readSquareOpt: Read[Option[Square]] = (value: String) => value.headOption.flatMap {
    case 'X' | 'O' | '.' => Some(Read[Square].read(value))
    case _ => None
  }

  implicit val showRow: Show[Row] = (t: Row) => Seq(t.col1.show, t.col2.show, t.col3.show).mkString(" ")

  implicit val readRow: Read[Row] = (value: String) => if (value.length == 5) {
    Applicative[Option].map3(
      Read[Option[Square]].read(value.charAt(0).toString),
      Read[Option[Square]].read(value.charAt(2).toString),
      Read[Option[Square]].read(value.charAt(4).toString)
    )(Row(_, _ , _))
  } else {
    None
  }
}
