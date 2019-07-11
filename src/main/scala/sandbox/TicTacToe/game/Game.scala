package sandbox.TicTacToe.game

import sandbox.TicTacToe.coordinates.{Col1, Col2, Col3, Coordinates, Row1, Row2, Row3}
import sandbox.TicTacToe.{O, Square, X}
import cats.instances.option._
import cats.syntax.semigroupk._

case class Game(row1: Row, row2: Row, row3: Row) {
    val full: Boolean = row1.full && row2.full && row3.full

    lazy val winner: Option[Square] = {
      def winnerRow(row: Row): Option[Square] = row match {
        case Row(Some(X), Some(X), Some(X)) => Some(X)
        case Row(Some(O), Some(O), Some(O)) => Some(O)
        case _ => None
      }

      def winnerColumn(colNr: Int): Option[Square] = if(colNr > 0 && colNr < 4) {
        winnerRow(colNr match {
          case 1 => Row(row1.col1, row2.col1, row3.col1)
          case 2 => Row(row1.col2, row2.col2, row3.col2)
          case 3 => Row(row1.col3, row2.col3, row3.col3)
        })
      } else {
        None
      }

      lazy val diagonalWinner = (row1, row2, row3) match {
        case (Row(Some(X), _, _), Row(_, Some(X), _), Row(_, _, Some(X))) => Some(X)
        case (Row(Some(O), _, _), Row(_, Some(O), _), Row(_, _, Some(O))) => Some(O)

        case (Row(_, _, Some(X)), Row(_, Some(X), _), Row(Some(X), _, _)) => Some(X)
        case (Row(_, _, Some(O)), Row(_, Some(O), _), Row(Some(O), _, _)) => Some(O)

        case _ => None
      }

      winnerRow(row1) <+>
        winnerRow(row2) <+>
        winnerRow(row3) <+>
        winnerColumn(1) <+>
        winnerColumn(2) <+>
        winnerColumn(3) <+>
        diagonalWinner
    }

    def +(move: Move): Option[Game] = move.coordinates match {
      case Coordinates(Col1, Row1) if row1.col1.isEmpty => Some(copy(row1 = row1.copy(col1 = Some(move.square))))
      case Coordinates(Col2, Row1) if row1.col2.isEmpty => Some(copy(row1 = row1.copy(col2 = Some(move.square))))
      case Coordinates(Col3, Row1) if row1.col3.isEmpty => Some(copy(row1 = row1.copy(col3 = Some(move.square))))

      case Coordinates(Col1, Row2) if row2.col1.isEmpty => Some(copy(row2 = row2.copy(col1 = Some(move.square))))
      case Coordinates(Col2, Row2) if row2.col2.isEmpty => Some(copy(row2 = row2.copy(col2 = Some(move.square))))
      case Coordinates(Col3, Row2) if row2.col3.isEmpty => Some(copy(row2 = row2.copy(col3 = Some(move.square))))

      case Coordinates(Col1, Row3) if row3.col1.isEmpty => Some(copy(row3 = row3.copy(col1 = Some(move.square))))
      case Coordinates(Col2, Row3) if row3.col2.isEmpty => Some(copy(row3 = row3.copy(col2 = Some(move.square))))
      case Coordinates(Col3, Row3) if row3.col3.isEmpty => Some(copy(row3 = row3.copy(col3 = Some(move.square))))

      case _ => None
    }
  }