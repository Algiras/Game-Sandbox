package sandbox

import cats.effect.ExitCode
import cats.instances.char._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import sandbox.Hangman.GameState._
import sandbox.typeClasses.Encoder._
import sandbox.typeClasses.{Console, Decoder}
import cats.syntax.flatMap._

package object Hangman {

  def outerGame[F[_] : Applicative : WordService]: PartialFunction[(GameState, Input), F[GameState]] = {
    case (_, Restart(difficulty)) => WordService[F].getWord.map(Game(difficulty.lives, _, Guesses.empty))
    case (_, ExitGame) => Applicative[F].pure(Exit)
  }

  val innerGame: PartialFunction[(GameState, Input), GameState] = {
    case (game@Game(lives, word, guesses), input: GameInput) if !Guesses.alreadyContains(guesses, input) => input match {
      case GuessWord(guessWord) if word == guessWord => Win(word)
      case GuessLetter(letter) if Guesses.isWordGuessedCorrectly(word, letter, guesses) => Win(word)

      case GuessWord(guessWord) if word != guessWord && Lives.sub(lives).isDefined => game.copy(lives = Lives.sub(lives).get, guesses = Guesses.addWord(guesses, guessWord))
      case GuessWord(guessWord) if word != guessWord && Lives.sub(lives).isEmpty => Lose(word, guesses = Guesses.addWord(guesses, guessWord))

      case GuessLetter(letter) if !word.toNes.contains(letter) && Lives.sub(lives).isDefined => game.copy(lives = Lives.sub(lives).get, guesses = Guesses.addLetter(guesses, letter))
      case GuessLetter(letter) if !word.toNes.contains(letter) && Lives.sub(lives).isEmpty => Lose(word, Guesses.addLetter(guesses, letter))
      case GuessLetter(letter) if word.toNes.contains(letter) => game.copy(guesses = Guesses.addLetter(guesses, letter))
    }
  }

  def hangmanFSM[F[_] : Applicative : WordService](state: GameState, input: Input): F[GameState] =
    outerGame orElse (innerGame andThen Applicative[F].pure) applyOrElse
      ((state, input), (_: (GameState, Input)) => Applicative[F].pure(state))

  def hangmanFSM[F[_] : Applicative : WordService](state: GameState, inputText: String): F[GameState] =
    Decoder[Input].decode(inputText).map(input => hangmanFSM(state, input)).getOrElse(Applicative[F].pure(state))

  def run[F[_] : Monad : WordService](implicit console: Console[F]): F[ExitCode] = {
    import console._

    def transition(state: GameState): F[GameState] = {
      (for {
        _ <- printLine(state.encode)
        gameInput <- readLine
        currentGameState <- hangmanFSM(state, gameInput)
      } yield currentGameState).tailRecM(_.map(game =>
        if (game == Exit)
          Right(game)
        else
          Left(transition(game))
      ))
    }

    transition(Init).map(_ => ExitCode.Success)
  }
}
