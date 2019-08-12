package sandbox.Hangman

import cats.data.{NonEmptyList, NonEmptySet}
import cats.instances.char._
import sandbox.Hangman.Guesses.{Letter, Word}
import sandbox.typeClasses.Encoder
import cats.instances.string._

case class Guesses(letters: Option[NonEmptySet[Letter]], words: Option[NonEmptySet[Word]])

object Guesses {
  type Letter = Char
  type Word = NonEmptyList[Letter]

  val empty = Guesses(None, None)

  def apply(letter: Letter): Guesses = Guesses(Some(NonEmptySet.of(letter)), None)
  def apply(word: Word): Guesses = Guesses(None, Some(NonEmptySet.of(word)))

  def addLetter(guesses: Guesses, letter: Letter): Guesses =
    guesses.copy(letters = guesses.letters.map(_.add(letter)).orElse(Some(NonEmptySet.of(letter))))
  def addWord(guesses: Guesses, word: Word): Guesses =
    guesses.copy(words = guesses.words.map(_.add(word)).orElse(Some(NonEmptySet.of(word))))

  def isWordGuessedCorrectly(word: Word, letter: Letter, guesses: Guesses): Boolean = word.toNes.toSortedSet
    .subsetOf(guesses.letters.map(_.add(letter)).getOrElse(NonEmptySet.of(letter)).toSortedSet)

  def alreadyContains(guesses: Guesses, letter: Letter): Boolean = guesses.letters.exists(_.exists(_ == letter))
  def alreadyContains(guesses: Guesses, word: Word): Boolean = guesses.words.exists(_.exists(_ == word))
  def alreadyContains(guesses: Guesses, gameInput: GameInput): Boolean = gameInput match {
    case GuessLetter(letter) => alreadyContains(guesses, letter)
    case GuessWord(word) => alreadyContains(guesses, word)
  }

  implicit val guessesEncoder: Encoder[Guesses] = (value: Guesses) => {
    def cmString[A](values: NonEmptySet[A]) = values.toNonEmptyList.toList.mkString(", ")

    "choices: " + ((value.letters, value.words) match {
      case (Some(letters), Some(words)) => cmString(letters) + ", " + cmString(words.map(_.toList.mkString("")))
      case (Some(letters), None) => cmString(letters)
      case (None, Some(words)) => cmString(words.map(_.toList.mkString("")))
      case _ => "No guesses yet"
    })
  }
}