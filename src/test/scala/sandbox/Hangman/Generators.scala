package sandbox.Hangman

import cats.data.{NonEmptyList, NonEmptySet}
import cats.instances.char._
import cats.instances.order._
import org.scalacheck.Gen.const
import org.scalacheck.{Arbitrary, Gen}
import sandbox.Hangman.Guesses.Letter
import sandbox.Hangman.Lives.Lives

import scala.collection.immutable.SortedSet

object Generators {
  val livesGen: Gen[Lives] = Gen.oneOf(const(Lives.avg), const(Lives.max))
  implicit val arbitraryLives: Arbitrary[Lives] = Arbitrary(livesGen)

  val guessesGen: Gen[Guesses] = for {
    letters <- Gen.option(Gen.nonEmptyListOf(Gen.alphaChar).map(letters => NonEmptySet[Letter](letters.head, SortedSet(letters.tail : _*))))
    words <- Gen.option(Gen.nonEmptyListOf(Gen.nonEmptyListOf(Gen.alphaChar))
      .map(words => NonEmptySet.fromSetUnsafe(SortedSet(words.map(word => NonEmptyList.fromListUnsafe(word)): _*))))
  } yield Guesses(letters, words)

  implicit val arbitraryGuesses: Arbitrary[Guesses] = Arbitrary(guessesGen)

  val wordGen: Gen[NonEmptyList[Char]] = Gen.nonEmptyListOf(Gen.alphaChar).map(NonEmptyList.fromListUnsafe)
  implicit val arbitraryWord: Arbitrary[NonEmptyList[Char]] = Arbitrary(wordGen)

  implicit val arbitraryState: Arbitrary[GameState] = {
    val genInit = const(Init)

    val genGame = for {
      lives <- livesGen
      word <- wordGen
      guesses <- guessesGen
    } yield Game(lives, word, guesses)

    val genWin = wordGen.flatMap(word => Win(word))

    val genLose = for {
      word <- wordGen
      guesses <- guessesGen
    } yield Lose(word, guesses)

    Arbitrary(Gen.oneOf(genInit, genWin, genLose, genGame))
  }

  implicit val arbitraryInput: Arbitrary[Input] = {
    val genExit = const(ExitGame)
    val genRestart = Gen.oneOf(const(Easy), const(Medium), const(Hard)).map(Restart)
    val genGuessWord = wordGen.map(GuessWord)
    val genGuessSymbol = Gen.asciiChar.map(GuessLetter)

    Arbitrary(Gen.oneOf(genExit, genRestart, genGuessWord, genGuessSymbol))
  }

  implicit val arbitraryDifficulty: Arbitrary[Difficulty] = Arbitrary(
    Gen.oneOf(const(Easy), const(Medium), const(Hard))
  )

  val arbitraryWordWithNonMatchingLetter: Arbitrary[(NonEmptyList[Char], Letter)] = Arbitrary(for {
    letter <- Gen.numChar
    word <- wordGen
    if !word.toNes.contains(letter)
  } yield (word, letter))

  val arbitraryWordWithMatchingLetter: Arbitrary[(NonEmptyList[Char], Letter)] = Arbitrary(for {
    letter <- Gen.alphaChar
    word <- wordGen
  } yield (letter :: word, letter))

  def unEvenPair[T: Arbitrary]: Arbitrary[(T, T)] = Arbitrary(Arbitrary.arbitrary[(T, T)].suchThat(tp => tp._1 != tp._2))

}
