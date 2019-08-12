package sandbox.Hangman

import sandbox.Hangman.Guesses.Word

trait WordService[F[_]] {
  def getWord: F[Word]
}

object WordService {
  def apply[F[_]](implicit ev: WordService[F]): WordService[F] = ev
}

