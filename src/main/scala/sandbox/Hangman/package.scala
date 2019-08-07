package sandbox

import cats.data.NonEmptySet
import cats.effect.ExitCode
import cats.instances.char._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import sandbox.Hangman.State._
import sandbox.typeClasses.Encoder._
import sandbox.typeClasses.{Console, Decoder}

package object Hangman {
  def hangmanFSM[F[_] : Applicative](service: WordService[F])(state: State, input: Input): F[State] = {
    def pure[A](value: A): F[A] = Applicative[F].pure[A](value)

    def addWordGuess(guesses: Option[Guesses], word: Word): Option[Guesses] =
      guesses.map(State.addWordGuess(_, word)).orElse(Some(wordChoice(word)))

    def addSymbolGuess(guesses: Option[Guesses], symbol: Char): Option[Guesses] =
      guesses.map(State.addSymbolGuess(_, symbol)).orElse(Some(symbolChoice(symbol)))

    def isWordGuessedCorrectly(word: Word, symbol: Char, guesses: Option[Guesses]) = word.toNes.toSortedSet
      .subsetOf(guesses.flatMap(_.left).map(_.add(symbol)).getOrElse(NonEmptySet.of(symbol)).toSortedSet)

    (state, input) match {
      case (Game(_, word, _), GuessWord(guessWord)) if word == guessWord => pure(Win(word))
      case (Game(_, word, guesses), GuessSymbol(symbol)) if isWordGuessedCorrectly(word, symbol, guesses) => pure(Win(word))

      case (game@Game(lives, word, guesses), GuessWord(guessWord)) if word != guessWord && lives.sub.isDefined =>
        pure(game.copy(lives = lives.sub.get, guesses = addWordGuess(guesses, guessWord)))
      case (Game(lives, word, guesses), GuessWord(guessWord)) if word != guessWord && lives.sub.isEmpty =>
        pure(Lose(word, guesses = addWordGuess(guesses, guessWord).get))

      case (game@Game(lives, word, guesses), GuessSymbol(symbol)) if !word.toNes.contains(symbol) && lives.sub.isDefined =>
        pure(game.copy(lives = lives.sub.get, guesses = addSymbolGuess(guesses, symbol)))
      case (Game(lives, word, guesses), GuessSymbol(symbol)) if !word.toNes.contains(symbol) && lives.sub.isEmpty =>
        pure(Lose(word, addSymbolGuess(guesses, symbol).get))
      case (game@Game(_, word, guesses), GuessSymbol(symbol)) if word.toNes.contains(symbol) =>
        pure(game.copy(guesses = addSymbolGuess(guesses, symbol)))

      case (_, Restart(difficulty)) => service.getWord.map(Game(difficulty.lives, _, None))
      case (_, ExitGame) => pure(Exit)

      case _ => pure(state)
    }
  }

  def run[F[_] : Monad](service: WordService[F])(implicit console: Console[F]): F[ExitCode] = {
    import console._

    val transitionFn: (State, Input) => F[State] = hangmanFSM(service)

    def transition(state: State): F[State] = for {
      _ <- printLine(state.encode)
      gameInput <- readLine.map(Decoder[Input].decode)
      currentGameState <- gameInput.map(transitionFn(state, _)).getOrElse(Monad[F].pure(state))
      recGme <- if (state == Exit) Monad[F].pure(state) else transition(currentGameState)
    } yield recGme

    transition(Init).map(_ => ExitCode.Success)
  }
}
