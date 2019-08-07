package sandbox.Hangman

import cats.Id
import cats.data.NonEmptyList
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import sandbox.Hangman.Generators._
import sandbox.Hangman.State._

class HangmanSpec extends Specification with ScalaCheck {
  val fakeWordService: WordService[Id] = new WordService[Id] {
    override def getWord: NonEmptyList[Char] = NonEmptyList.fromListUnsafe("word".toList)
  }

  def gameFSM(state: State, input: Input): State = hangmanFSM[Id](fakeWordService)(state, input)

  "Hangman" should {
    "fsm" should {
      "win the game if user chooses the word correctly" >> prop { (lives: Lives, word: NonEmptyList[Char]) =>
        gameFSM(Game(lives, word, None), GuessWord(word)) must_=== Win(word)
      }

      "lose one life if you choose incorrect word as your answer" >> {
        prop { (lives: Lives, guess: (NonEmptyList[Char], NonEmptyList[Char])) =>
          val game = Game(lives, guess._1, None)
          gameFSM(game, GuessWord(guess._2)) must_=== game.copy(lives = lives.sub.get, guesses = Some(wordChoice(guess._2)))
        }
        .setArbitrary2(unEvenPair[NonEmptyList[Char]])
      }

      "lose game if after incorrect word guess your lives drop to 0" >> prop { guess: (NonEmptyList[Char], NonEmptyList[Char]) =>
          val game = Game(Lives.min, guess._1, None)
          gameFSM(game, GuessWord(guess._2)) must_=== Lose(guess._1, wordChoice(guess._2))
        }.setArbitrary(unEvenPair[NonEmptyList[Char]])

      "win the game if user chooses the symbol correctly" >> prop { (lives: Lives, headSymbol: Char, word: NonEmptyList[Char]) =>
        gameFSM(Game(lives, headSymbol :: word, Some(symbolChoices(word))), GuessSymbol(headSymbol)) must_=== Win(headSymbol :: word)
      }.setArbitrary3(arbitraryWord)


      "progress in the game without losing life when player chooses symbol correctly" >> prop { (lives: Lives, wordAndCorrectSymbol: (NonEmptyList[Char], Char), additionalSymbol: Char) =>
        val word = additionalSymbol :: wordAndCorrectSymbol._1

        gameFSM(
          Game(lives, word, None), GuessSymbol(wordAndCorrectSymbol._2)
        ) must_=== Game(lives, word, Some(symbolChoice(wordAndCorrectSymbol._2)))

        }.setArbitrary1(arbitraryLives)
        .setArbitrary2(arbitraryWordWithMatchingSymbol)

      "lose one life if you choose incorrect symbol as an answer" >> prop { (lives: Lives, guess: (NonEmptyList[Char], Char)) =>
          val game = Game(lives, guess._1, None)
          gameFSM(game, GuessSymbol(guess._2)) must_=== game.copy(lives = game.lives.sub.get, guesses = Some(symbolChoice(guess._2)))
        }
        .setArbitrary1(arbitraryLives)
        .setArbitrary2(arbitraryWordWithNonMatchingSymbol)


      "lose the game if you choose incorrect symbol as an answer" >> prop { guess: (NonEmptyList[Char], Char) =>
          val game = Game(Lives.min, guess._1, None)
          gameFSM(game, GuessSymbol(guess._2)) must_=== Lose(guess._1, symbolChoice(guess._2))
        }
          .setArbitrary(arbitraryWordWithNonMatchingSymbol)

      "running exit game on any state, goes to Exit state" >> prop { state: State =>
        gameFSM(state, ExitGame) must_=== Exit
      }

      "running restart game on any state, restart the game and start a new one" >> prop { (newWord: NonEmptyList[Char], state: State, difficulty: Difficulty) =>
        val fakeWordService: WordService[Id] = new WordService[Id] {
          override def getWord: NonEmptyList[Char] = newWord
        }

        hangmanFSM(fakeWordService)(state, Restart(difficulty)) must_=== Game(difficulty.lives, newWord, None)
      }.setArbitrary2(Arbitrary(arbitraryState.arbitrary))
    }
  }
}
