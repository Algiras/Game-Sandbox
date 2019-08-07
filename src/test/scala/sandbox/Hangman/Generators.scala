package sandbox.Hangman

import cats.data.{Ior, NonEmptyList, NonEmptySet}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.const
import cats.instances.char._
import State.{symbolChoices, _}

object Generators {
  val livesGen: Gen[Lives] = Gen.oneOf(const(Lives.max), const(Lives.avg))
  implicit val arbitraryLives: Arbitrary[Lives] = Arbitrary(livesGen)

  val guessesGen: Gen[Ior[NonEmptySet[Char], NonEmptySet[NonEmptyList[Char]]]] = for {
    symbols <- Gen.nonEmptyListOf(Gen.alphaChar).map(ls => symbolChoices(NonEmptyList.fromListUnsafe(ls)))
    words <- Gen.nonEmptyListOf(Gen.nonEmptyListOf(Gen.alphaChar)).map(ls => wordChoices(NonEmptyList.fromListUnsafe(ls.map(NonEmptyList.fromListUnsafe))))
  } yield symbols.combine(words)
  implicit val arbitraryGuesses: Arbitrary[Ior[NonEmptySet[Char], NonEmptySet[NonEmptyList[Char]]]] = Arbitrary(guessesGen)

  val wordGen: Gen[NonEmptyList[Char]] = Gen.nonEmptyListOf(Gen.alphaChar).map(NonEmptyList.fromListUnsafe)
  implicit val arbitraryWord: Arbitrary[NonEmptyList[Char]] = Arbitrary(wordGen)

  implicit val arbitraryState: Arbitrary[State] = {
    val genInit = const(Init)

    val genGame = for {
      lives <- livesGen
      word <- wordGen
      guesses <- Gen.option(Gen.nonEmptyListOf(Gen.asciiChar))
    } yield Game(lives, word, guesses.map(choices => symbolChoices(NonEmptyList.fromListUnsafe(choices))))

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
    val genGuessSymbol = Gen.asciiChar.map(GuessSymbol)

    Arbitrary(Gen.oneOf(genExit, genRestart, genGuessWord, genGuessSymbol))
  }

  implicit val arbitraryDifficulty: Arbitrary[Difficulty] = Arbitrary(
    Gen.oneOf(const(Easy), const(Medium), const(Hard))
  )

  val arbitraryWordWithNonMatchingSymbol: Arbitrary[(NonEmptyList[Char], Char)] = Arbitrary(for {
    symbol <- Gen.numChar
    word <- wordGen
    if !word.toNes.contains(symbol)
  } yield (word, symbol))

  val arbitraryWordWithMatchingSymbol: Arbitrary[(NonEmptyList[Char], Char)] = Arbitrary(for {
    symbol <- Gen.alphaChar
    word <- wordGen
  } yield (symbol :: word, symbol))

  def unEvenPair[T: Arbitrary]: Arbitrary[(T, T)] = Arbitrary(Arbitrary.arbitrary[(T, T)].suchThat(tp => tp._1 != tp._2))

}
