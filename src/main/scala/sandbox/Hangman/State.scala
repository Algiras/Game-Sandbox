package sandbox.Hangman

import cats.data.Ior.Both
import cats.data.{Ior, NonEmptyList, NonEmptySet}
import sandbox.typeClasses.Encoder
import sandbox.typeClasses.Encoder._
import cats.instances.string._
import cats.instances.char._
import cats.syntax.ior._
import sandbox.Hangman.State.{Guesses, Word}


sealed trait State

case object Init extends State

case object Exit extends State

case class Game(lives: Lives, word: Word, guesses: Option[Guesses]) extends State

case class Win(word: Word) extends State

case class Lose(word: Word, guesses: Guesses) extends State


object State {
  type Word = NonEmptyList[Char]
  type Guesses = NonEmptySet[Char] Ior NonEmptySet[NonEmptyList[Char]]

  def wordChoice(wordChoice: Word): Guesses = NonEmptySet.of(wordChoice).rightIor
  def symbolChoice(symbolChoice: Char): Guesses = NonEmptySet.of(symbolChoice).leftIor

  def symbolChoices(symbolChoices: NonEmptyList[Char]): Guesses = symbolChoices.toNes.leftIor
  def wordChoices(symbolChoices: NonEmptyList[Word]): Guesses = symbolChoices.toNes.rightIor

  def addSymbolGuess(choices: Guesses, guess: Char): Guesses = choices match {
    case Ior.Left(symbols) => symbols.add(guess).leftIor
    case Ior.Right(words) => Ior.both(NonEmptySet.of(guess), words)
    case Both(symbols, words) => Ior.both(symbols.add(guess), words)
  }

  def addWordGuess(choices: Guesses, guess: NonEmptyList[Char]): Guesses = choices match {
    case Ior.Left(symbols) => Ior.both(symbols, NonEmptySet.of(guess))
    case Ior.Right(words) => words.add(guess).rightIor
    case Both(symbols, words) => Ior.both(symbols, words.add(guess))
  }

  val emptyGameScene: Vector[Vector[String]] = {
    val game = """
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

    game.map(line => if(maxLength == line.length) line else line.padTo(maxLength, " "))
  }

  val emptyGame: String = emptyGameScene.map(_.mkString("")).mkString("\n")

  implicit val lifeEncoder: Encoder[Lives] = new Encoder[Lives] {

    case class Piece(position: (Int, Int), symbol: String)

    val pieces = Vector(
      Piece((5, 9), "\\"),
      Piece((5, 7), "/"),
      Piece((4, 8), "|"),
      Piece((3, 9), "\\"),
      Piece((3, 7), "/"),
      Piece((3, 8), "|"),
      Piece((2, 9), ")"),
      Piece((2, 8), "_"),
      Piece((2, 7), "("),
      Piece((1, 8), "|"),
    )

    override def encode(value: Lives): String = pieces.drop(value.lives).foldLeft(emptyGameScene) {
      case (drawing, Piece((x, y), symbol)) => drawing.updated(x, drawing(x).updated(y, symbol))
    }.map(_.mkString("")).mkString("\n")
  }

  def guessesToStr(guesses: Guesses): String = guesses
    .leftMap(_.map(NonEmptyList.of(_)))
    .fold(identity, identity, _ ++ _)
    .map(_.toList.mkString(""))
    .toSortedSet
    .mkString(", ")


  val info: String = List(
    "Available options:",
    List[Input](
      ExitGame,
      Restart(Easy),
      Restart(Medium),
      Restart(Hard)
    ).map(_.encode).mkString("\n"),
  ).mkString("\n")

  implicit val stateEncoder: Encoder[State] = {
    case Init => info
    case Win(word) => s"Congratulations, you won. The word was: ${word.toList.mkString("")}"
    case Lose(word, guesses) => s"You lost. The word was ${word.toList.mkString("")} and your guesses where ${guessesToStr(guesses)}"
    case Game(lives, word, guesses) => List(
      List(lives.encode),
      List(s"word: ${word.map(chr => if (guesses.flatMap(_.left).exists(_.exists(_ == chr))) chr else '_').toList.mkString(" ")}"),
      guesses.toList.flatMap(chs => List(s"choices: ${guessesToStr(chs)}")),
      List(s"lives: ${lives.lives}")
    ).flatten.mkString("\n")
    case Exit => "Exiting Hangman game..."
  }
}
