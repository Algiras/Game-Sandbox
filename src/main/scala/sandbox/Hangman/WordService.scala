package sandbox.Hangman

import cats.data.NonEmptyList

trait WordService[F[_]] {
  def getWord: F[NonEmptyList[Char]]
}


