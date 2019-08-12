package sandbox.Hangman

import sandbox.Hangman.Guesses.Word
import sandbox.typeClasses.Encoder
import sandbox.typeClasses.Encoder._
import Guesses._
import sandbox.Hangman.Lives.Lives

sealed trait GameState

case object Init extends GameState

case object Exit extends GameState

case class Game(lives: Lives, word: Word, guesses: Guesses) extends GameState

case class Win(word: Word) extends GameState

case class Lose(word: Word, guesses: Guesses) extends GameState


object GameState {

  private[Hangman] val emptyGameScene  = {
    val game =
      """
        |_________
        |/
        |
        |
        |
        |
        |
        |_________"""
        .split("\n")
        .drop(1)
        .toVector.map(_.dropWhile(_ != '|').split("").toVector)

    val maxLength = game.map(_.length).max

    game.map(line => if (maxLength == line.length) line else line.padTo(maxLength, " "))
  }

  implicit val lifeEncoder: Encoder[Lives] = new Encoder[Lives] {

    case class Piece(position: (Int, Int), letter: String)

    val offset: (Int, Int) = (1, 7)

    val pieces: Vector[Piece] =
      """| |
         |(_)
         |/|\
         | |
         |/ \""".stripMargin.split("\n").zipWithIndex.flatMap {
        case (line, x) => line.zipWithIndex.filterNot(_._1 == ' ').map {
          case (letter, y) => Piece((x + offset._1, y + offset._2), letter.toString)
        }
      }.toVector.reverse

    override def encode(value: Lives): String = pieces.drop(value).foldLeft(emptyGameScene) {
      case (drawing, Piece((x, y), letter)) => drawing.updated(x, drawing(x).updated(y, letter))
    }.map(_.mkString("")).mkString("\n")
  }

  val info: String = List(
    "Available options:",
    List[Input](
      ExitGame,
      Restart(Easy),
      Restart(Medium),
      Restart(Hard)
    ).map(_.encode).mkString("\n"),
  ).mkString("\n")

  implicit val stateEncoder: Encoder[GameState] = {
    case Init => info
    case Win(word) => s"Congratulations, you won. The word was: ${word.toList.mkString("")}"
    case Lose(word, guesses) => s"You lost. The word was ${word.toList.mkString("")} and your guesses where ${guesses.encode}"
    case Game(lives, word, guesses) => List(
      List(lives.encode),
      List(s"word: ${word.map(chr => if (guesses.letters.exists(_.exists(_ == chr))) chr else '_').toList.mkString(" ")}"),
      List(guesses.encode),
      List(s"lives: $lives")
    ).flatten.mkString("\n")
    case Exit => "Exiting Hangman game..."
  }
}
