package sandbox


import cats.effect.ExitCode
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import sandbox.TicTacToe.coordinates.{Col1, Col2, Col3, ColumnCoordinate, Coordinates, Row1, Row2, Row3, RowCoordinate}
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

  def run[F[_]: Monad](implicit console: Console[F]): F[ExitCode] = {

    def retry[T](readValue: F[Option[T]]): F[T] = for {
      element <- readValue
      getOrRetry <- if(element.isDefined) Applicative[F].pure(element.get) else console.printLine(Text.invalidChoice) *> retry(readValue)
    } yield getOrRetry

    def readGame(square: Square, game: Game): F[Game] = for {
      _ <- console.printLine(displayGameOnConsole(game))
      _ <- console.printLine(Text.nextMove(square))
      _ <- console.printLine(Text.validInputs)
      nextStage <- retry(console.readLine.map(Decoder[Coordinates].decode).map(cord => cord.flatMap(coordinates => game + Move(square, coordinates))))
    } yield nextStage

    def gameLoop(square: Square, game: Game): F[Option[Square]] = for {
      currentGame <- readGame(square, game)
      nextGame <- if(currentGame.full) Applicative[F].pure(None: Option[Square]) else if(currentGame.winner.isDefined) Applicative[F].pure(currentGame.winner) else gameLoop(Square.opposite(square), currentGame)
    } yield nextGame

    lazy val readSquare: F[Square] = retry(console.readLine.map(Decoder[Square].decode))

    for {
      _ <- console.printLine(Text.chooseText(X))
      square <- readSquare
      result <- gameLoop(square, game.empty)
      _ <- console.printLine(result.map(Text.winner).getOrElse(Text.draw))
    } yield ExitCode.Success
  }
}
