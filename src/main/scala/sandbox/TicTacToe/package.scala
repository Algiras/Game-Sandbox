package sandbox


import cats.effect.ExitCode
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import sandbox.TicTacToe.coordinates.{Col1, Col2, Col3, ColumnCoordinate, Coordinate, Row1, Row2, Row3, RowCoordinate}
import sandbox.TicTacToe.game.Row._
import sandbox.TicTacToe.game.{Game, Move, Row}
import sandbox.TicTacToe.typeClasses.Encoder._
import sandbox.TicTacToe.typeClasses.{Console, Decoder}

package object TicTacToe {

  object Text {
    val invalidChoice = "Invalid choice, try again"
    val draw = "It was a draw"

    def winner(square: Square) = s"${square.encode} won"

    def nextMove(square: Square) = s"Your next move with ${square.encode}:"

    def chooseText(square: Square) = s"Choose initial symbol: ${square.encode} or ${Square.opposite(square).encode}"

    val validInputs: String = coordinates.combinations.map(_.encode).mkString(" ")
  }

  def displayGameOnConsole(game: Game): String = {
    def encodeRow(row: Row, coordinate: RowCoordinate) = s"${coordinate.encode} ${row.encode}"

    Seq(
      "  " + Seq[ColumnCoordinate](Col1, Col2, Col3).map(_.encode).mkString(" "),
      encodeRow(game.row1, Row1),
      encodeRow(game.row2, Row2),
      encodeRow(game.row3, Row3)
    ).mkString("\n")
  }

  def run[F[_] : Monad](implicit console: Console[F], applicative: Applicative[F]): F[ExitCode] = {
    import console._, applicative._

    def retry[T](readValue: F[Option[T]]): F[T] = readValue.flatMap {
      case Some(value) => pure(value)
      case None => printLine(Text.invalidChoice) *> retry(readValue)
    }

    def readGame(square: Square, currentGame: Game): F[Game] = for {
      _ <- printLine(displayGameOnConsole(currentGame))
      _ <- printLine(Text.nextMove(square))
      _ <- printLine(Text.validInputs)
      nextStage <- retry(readLine.map(Decoder[Coordinate].decode).map(_.flatMap(coordinates => game.move(currentGame, Move(square, coordinates)))))
    } yield nextStage

    def gameLoop(square: Square, gameState: Game): F[Option[Square]] = readGame(square, gameState).flatMap { currentGame =>
      if (game.full(currentGame))
        pure(None)
      else
        game.winner(currentGame) match {
          case result@Some(_) => pure(result)
          case None => gameLoop(Square.opposite(square), currentGame)
        }
    }

    for {
      _ <- printLine(Text.chooseText(X))
      square <- retry(readLine.map(Decoder[Square].decode))
      result <- gameLoop(square, game.empty)
      _ <- printLine(result.map(Text.winner).getOrElse(Text.draw))
    } yield ExitCode.Success
  }
}
