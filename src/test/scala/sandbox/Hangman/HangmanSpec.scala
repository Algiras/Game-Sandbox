package sandbox.Hangman

import cats.Id
import cats.data.NonEmptyList
import org.scalacheck.Arbitrary
import org.specs2.{ScalaCheck, Specification}
import sandbox.Hangman.Generators._
import sandbox.Hangman.Lives.Lives

class HangmanSpec extends Specification with ScalaCheck {
  def is =
    s2"""
      Hangman Specification needs to follow general rules provided https://en.wikipedia.org/wiki/Hangman_(game)

      Winning the game:
       win the game if user chooses the word correctly                                       $winGameWord
       win the game if you choose the correct letter                                         $validLetterWinGame

      Loosing the game(only applies if lives are 1):
       lose the game if you choose incorrect letter as an answer                             $loseGameBadLetter
       lose the game if you choose incorrect word as an answer                               $loseGameBadWord

      Loosing lives:
       lose one life if you choose incorrect word as your answer                             $loseLifeBadWord
       lose one life if you choose incorrect letter as your answer                           $loseLifeBadLetter

      Progressing in the game:
       progress in the game without losing lives when player chooses letter correctly        $gameProgressLetter

      Game mechanics:
       running `exit`  on any state, exits the game(goes to exit state)                      $exitingGame
       running `restart {difficulty}` on any state, restart the game                         $restartGame

  """

  implicit val fakeWordService: WordService[Id] = new WordService[Id] {
    override def getWord: NonEmptyList[Char] = NonEmptyList.fromListUnsafe("word".toList)
  }

  def gameFSM(state: GameState, input: Input): GameState = hangmanFSM[Id](state, input)

  def winGameWord = prop { (lives: Lives, word: NonEmptyList[Char]) =>
    gameFSM(Game(lives, word, Guesses.empty), GuessWord(word)) must_=== Win(word)
  }

  def loseLifeBadWord = prop { (lives: Lives, guess: (NonEmptyList[Char], NonEmptyList[Char])) =>
    val game = Game(lives, guess._1, Guesses.empty)
    gameFSM(game, GuessWord(guess._2)) must_=== game.copy(lives = Lives.sub(lives).get, guesses = Guesses(guess._2))
  }
    .setArbitrary2(unEvenPair[NonEmptyList[Char]])


  def loseGameBadWord = prop { guess: (NonEmptyList[Char], NonEmptyList[Char]) =>
    val game = Game(Lives.min, guess._1, Guesses.empty)
    gameFSM(game, GuessWord(guess._2)) must_=== Lose(guess._1, Guesses(guess._2))
  }.setArbitrary(unEvenPair[NonEmptyList[Char]])

  def validLetterWinGame = prop { (lives: Lives, headSymbol: Char, word: NonEmptyList[Char]) =>
    gameFSM(Game(lives, headSymbol :: word, word.foldLeft(Guesses.empty)(Guesses.addLetter)), GuessLetter(headSymbol)) must_=== Win(headSymbol :: word)
  }.setArbitrary3(arbitraryWord)

  def gameProgressLetter = prop { (lives: Lives, wordAndCorrectSymbol: (NonEmptyList[Char], Char), additionalSymbol: Char) =>
    val word = additionalSymbol :: wordAndCorrectSymbol._1

    gameFSM(
      Game(lives, word, Guesses.empty), GuessLetter(wordAndCorrectSymbol._2)
    ) must_=== Game(lives, word, Guesses(wordAndCorrectSymbol._2))

  }.setArbitrary1(arbitraryLives)
    .setArbitrary2(arbitraryWordWithMatchingLetter)

  def loseLifeBadLetter = prop { (lives: Lives, guess: (NonEmptyList[Char], Char)) =>
    val game = Game(lives, guess._1, Guesses.empty)
    gameFSM(game, GuessLetter(guess._2)) must_=== game.copy(lives = Lives.sub(game.lives).get, guesses = Guesses(guess._2))
  }
    .setArbitrary1(arbitraryLives)
    .setArbitrary2(arbitraryWordWithNonMatchingLetter)

  def loseGameBadLetter = prop { guess: (NonEmptyList[Char], Char) =>
    val game = Game(Lives.min, guess._1, Guesses.empty)
    gameFSM(game, GuessLetter(guess._2)) must_=== Lose(guess._1, Guesses(guess._2))
  }
    .setArbitrary(arbitraryWordWithNonMatchingLetter)


  def exitingGame = prop { state: GameState =>
    gameFSM(state, ExitGame) must_=== Exit
  }

  def restartGame = prop { (newWord: NonEmptyList[Char], state: GameState, difficulty: Difficulty) =>
    implicit val fakeWordService: WordService[Id] = new WordService[Id] {
      override def getWord: NonEmptyList[Char] = newWord
    }

    hangmanFSM(state, Restart(difficulty)) must_=== Game(difficulty.lives, newWord, Guesses.empty)
  }.setArbitrary2(Arbitrary(arbitraryState.arbitrary))
}
