package sandbox.Hangman

import cats.data.{NonEmptyList, StateT}
import org.specs2.mutable.Specification
import sandbox.Hangman.Guesses.{Letter, Word}
import sandbox.TestCase
import sandbox.TestCase._
import cats.instances.either._
import cats.syntax.either._
import sandbox.Hangman.GameState._
import sandbox.typeClasses.Encoder._

class HangmanITSpec extends Specification {
  val word: NonEmptyList[Letter] = NonEmptyList.fromListUnsafe("bird".toList)

  implicit val wordService: WordService[TestState] = new WordService[TestState]{
    override def getWord: TestState[Word] = StateT[Either[String, ?], TestCase, Word](
      testCase => (testCase, word).asRight[String]
    )
  }

  def guesses(letters: Letter*): Guesses = letters.foldLeft(Guesses.empty)(Guesses.addLetter)

  "Hangman" should {
    "easily win the game" >> {
      run[TestState].run(TestCase(
        Seq[Input](
          Restart(Easy),
          GuessLetter('b'),
          GuessLetter('i'),
          GuessLetter('r'),
          GuessLetter('d'),
          ExitGame).map(_.encode),
        Seq[GameState](
          Init,
          Game(Lives.max, word, Guesses.empty),
          Game(Lives.max, word, guesses('b')),
          Game(Lives.max, word, guesses('b', 'i')),
          Game(Lives.max, word, guesses('b', 'i', 'r')),
          Win(word)
        ).map(_.encode)
      )) must beRight(completedProgram)
    }

    "struggle with one a letter" >> {
      run[TestState].run(TestCase(
        Seq[Input](
          Restart(Easy),
          GuessLetter('b'),
          GuessLetter('i'),
          GuessLetter('r'),
          GuessLetter('t'),
          GuessLetter('d'),
          ExitGame).map(_.encode),
        Seq[GameState](
          Init,
          Game(Lives.max, word, Guesses.empty),
          Game(Lives.max, word, guesses('b')),
          Game(Lives.max, word, guesses('b', 'i')),
          Game(Lives.max, word, guesses('b', 'i', 'r')),
          Game(Lives.sub(Lives.max).get, word, guesses('b', 'i', 'r', 't')),
          Win(word)
        ).map(_.encode)
      )) must beRight(completedProgram)
    }

    "figure our the word ar the end" >> {
      run[TestState].run(TestCase(
        Seq[Input](
          Restart(Easy),
          GuessLetter('b'),
          GuessLetter('i'),
          GuessLetter('r'),
          GuessWord(word),
          ExitGame).map(_.encode),
        Seq[GameState](
          Init,
          Game(Lives.max, word, Guesses.empty),
          Game(Lives.max, word, guesses('b')),
          Game(Lives.max, word, guesses('b', 'i')),
          Game(Lives.max, word, guesses('b', 'i', 'r')),
          Win(word)
        ).map(_.encode)
      )) must beRight(completedProgram)
    }
  }
}
