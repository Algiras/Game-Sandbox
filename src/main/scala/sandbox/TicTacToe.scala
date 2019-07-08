package sandbox

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.show._
import cats.syntax.semigroupk._
import cats.syntax.apply._
import cats.Show
import cats.Applicative
import cats.instances.option._

object TicTacToe extends IOApp {
  trait Read[T] {
    def read(value: String): Option[T]
  }

  object Read {
    def apply[T](implicit ev: Read[T]): Read[T] = ev
  }

  implicit val showSquare: Show[Square] = {
    case X => "X"
    case O => "O"
  }

  implicit val showSquareOpt: Show[Option[Square]] = (t: Option[Square]) => t.map(_.show).getOrElse(".")

  implicit val readSquare: Read[Square] = (value: String) => value.headOption.flatMap {
    case 'X' => Some(X)
    case 'O' => Some(O)
    case _ => None
  }

  implicit val readSquareOpt: Read[Option[Square]] = (value: String) => value.headOption.flatMap {
    case 'X' | 'O' | '.' => Some(Read[Square].read(value))
    case _ => None
  }

  sealed trait Square

  object Square {
    def opposite(value: Square): Square = value match {
      case X => O
      case O => X
    }
  }

  final object X extends Square
  final object O extends Square

  case class Row(col1: Option[Square], col2: Option[Square], col3: Option[Square]) {
    val full: Boolean = col1.isDefined && col2.isDefined && col3.isDefined
  }
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

  case class Move(square: Square, coordinates: Coordinates)

  object Game {
    def apply(row1: Row, row2: Row, row3: Row): Game = new Game(row1, row2, row3)

    val empty = Game(
      Row(None, None, None),
      Row(None, None, None),
      Row(None, None, None)
    )
  }

  implicit val showRow: Show[Row] = (t: Row) => Seq(t.col1.show, t.col2.show, t.col3.show).mkString(" ")

  implicit val readRow: Read[Row] = (value: String) => if (value.length == 5) {
    Applicative[Option].map3(
      Read[Option[Square]].read(value.charAt(0).toString),
      Read[Option[Square]].read(value.charAt(2).toString),
      Read[Option[Square]].read(value.charAt(4).toString)
    )(Row)
  } else {
    None
  }

  implicit val readGame: Read[Game] = (value: String) => {
    val lines = value.split('\n')

    if (lines.length == 3) {
      Applicative[Option].map3(
        Read[Row].read(lines(0)),
        Read[Row].read(lines(1)),
        Read[Row].read(lines(2))
      )(Game(_, _, _))
    } else {
      None
    }
  }

  implicit val showGame: Show[Game] = (t: Game) => Seq(t.row1.show, t.row2.show, t.row3.show).mkString("\n")

  sealed trait RowCoordinate
  final case object Row1 extends RowCoordinate
  final case object Row2 extends RowCoordinate
  final case object Row3 extends RowCoordinate

  sealed trait ColumnCoordinate
  final case object Col1 extends ColumnCoordinate
  final case object Col2 extends ColumnCoordinate
  final case object Col3 extends ColumnCoordinate

  case class Coordinates(column: ColumnCoordinate, row: RowCoordinate)

  object Coordinates {
    def apply(column: ColumnCoordinate, row: RowCoordinate): Coordinates = new Coordinates(column, row)

    val A1 = Coordinates(Col1, Row1)
    val A2 = Coordinates(Col1, Row2)
    val A3 = Coordinates(Col1, Row3)
    val B1 = Coordinates(Col2, Row1)
    val B2 = Coordinates(Col2, Row2)
    val B3 = Coordinates(Col2, Row3)
    val C1 = Coordinates(Col3, Row1)
    val C2 = Coordinates(Col3, Row2)
    val C3 = Coordinates(Col3, Row3)
  }

  implicit val showRowCoordinates = new Show[RowCoordinate] {
    override def show(t: RowCoordinate): String = t match {
      case Row1 => "1"
      case Row2 => "2"
      case Row3 => "3"
    }
  }

  implicit val readRowCoordinates = new Read[RowCoordinate] {
    override def read(value: String): Option[RowCoordinate] = value match {
      case "1" => Some(Row1)
      case "2" => Some(Row2)
      case "3" => Some(Row3)
      case _ => None
    }
  }

  implicit val showColumnCoordinates = new Show[ColumnCoordinate] {
    override def show(t: ColumnCoordinate): String = t match {
      case Col1 => "A"
      case Col2 => "B"
      case Col3 => "C"
    }
  }


  implicit val readColumnCoordinates = new Read[ColumnCoordinate] {
    override def read(value: String): Option[ColumnCoordinate] = value match {
      case "A" => Some(Col1)
      case "B" => Some(Col2)
      case "C" => Some(Col3)
      case _ => None
    }
  }

  implicit val showCoordinates = new Show[Coordinates] {
    override def show(t: Coordinates): String = t.column.show + t.row.show
  }

  implicit val readCoordinates = new Read[Coordinates] {
    override def read(value: String): Option[Coordinates] = if(value.length == 2) {
      Applicative[Option].map2(
        Read[ColumnCoordinate].read(value.charAt(0).toString),
        Read[RowCoordinate].read(value.charAt(1).toString)
      )(Coordinates(_, _))
    }  else {
      None
    }
  }


  def displayGameOnConsole(game: Game): String = {
    def showRow(row: Row, coordinate: RowCoordinate) = s"${coordinate.show} " + row.show

    Seq(
      "  " + Seq[ColumnCoordinate](Col1, Col2, Col3).map(_.show).mkString(" "),
      showRow(game.row1, Row1),
      showRow(game.row2, Row2),
      showRow(game.row3, Row3)
    ).mkString("\n")
  }

  trait Console[F[_]] {
    def readLine: F[String]
    def printLine(text: String): F[Unit]
  }

  object Console {
    def apply[F[_]](implicit ev: Console[F]): Console[F] = ev
  }

  implicit val ioInstance: Console[IO] = new Console[IO] {
    import scala.io.StdIn

    override def readLine: IO[String] = IO(StdIn.readLine)
    override def printLine(text: String): IO[Unit] = IO(println(text))
  }

  override def run(args: List[String]): IO[ExitCode] = {
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
      result <- gameLoop(square, Game.empty)
      _ <- if(result.isEmpty) console.printLine("It was a draw") else console.printLine(s"${result.get.show} won")
    } yield ExitCode.Success
    // Choose Square
    // Start Game
    // Check if anyone won or game complete
    // Show result
    // Restart the game
  }
}
