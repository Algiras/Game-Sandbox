package sandbox


import cats.effect.{ExitCode, IO}
import cats.syntax.apply._
import cats.syntax.show._
import sandbox.TicTacToe.coordinates.{Col1, Col2, Col3, ColumnCoordinate, Coordinates, Row1, Row2, Row3, RowCoordinate}
import sandbox.TicTacToe.game.{Game, Move, Row}
import sandbox.TicTacToe.typeClasses.{Console, Read}

package object TicTacToe {
  def displayGameOnConsole(game: Game): String = {
    def showRow(row: Row, coordinate: RowCoordinate) = s"${coordinate.show} " + row.show

    Seq(
      "  " + Seq[ColumnCoordinate](Col1, Col2, Col3).map(_.show).mkString(" "),
      showRow(game.row1, Row1),
      showRow(game.row2, Row2),
      showRow(game.row3, Row3)
    ).mkString("\n")
  }

  implicit val ioInstance: Console[IO] = new Console[IO] {
    import scala.io.StdIn

    override def readLine: IO[String] = IO(StdIn.readLine)
    override def printLine(text: String): IO[Unit] = IO(println(text))
  }

  def run(args: List[String]): IO[ExitCode] = {
    val console = Console[IO]

    def retry[T](readValue: IO[Option[T]]): IO[T] = for {
      element <- readValue
      getOrRetry <- if(element.isDefined) IO(element.get) else console.printLine("Invalid choice, try again") *> retry(readValue)
    } yield getOrRetry

    def readGame(square: Square, game: Game): IO[Game] = for {
      _ <- console.printLine(displayGameOnConsole(game))
      _ <- console.printLine(s"Your next move with ${square.show}:")
      _ <- console.printLine("A1 A2 A3 B1 B2 B3 C1 C2 C3")
      nextStage <- retry(console.readLine.map(Read[Coordinates].read).map(cord => cord.flatMap(coordinates => game + Move(square, coordinates))))
    } yield nextStage

    def gameLoop(square: Square, game: Game): IO[Option[Square]] = for {
      currentGame <- readGame(square, game)
      nextGame <- if(currentGame.full) IO(None) else if(currentGame.winner.isDefined) IO.pure(currentGame.winner) else gameLoop(Square.opposite(square), currentGame)
    } yield nextGame

    lazy val readSquare: IO[Square] = retry(console.readLine.map(Read[Square].read))

    for {
      _ <- console.printLine("Choose initial symbol: X or O")
      square <- readSquare
      result <- gameLoop(square, game.empty)
      _ <- if(result.isEmpty) console.printLine("It was a draw") else console.printLine(s"${result.get.show} won")
    } yield ExitCode.Success
  }
}
