package sandbox.Hangman

import cats.data.NonEmptyList
import sandbox.Hangman.Difficulty._
import sandbox.Hangman.State.Word
import sandbox.typeClasses.{Decoder, Encoder}
import sandbox.typeClasses.Encoder._

sealed trait Input

case object ExitGame extends Input

case class Restart(difficulty: Difficulty) extends Input

case class GuessWord(guess: Word) extends Input

case class GuessSymbol(guess: Char) extends Input

object Input {
  implicit val inputEncoder: Encoder[Input] = {
    case ExitGame => ":exit"
    case Restart(difficulty) => s":restart ${difficulty.encode}"
    case GuessWord(word) => s"Your input was: `$word`"
    case GuessSymbol(symbol) => s"Your input was: `$symbol`"
  }

  implicit val inputDecoder: Decoder[Input] = new Decoder[Input] {
    override def decode(value: String): Option[Input] = value match {
      case _ if value.headOption.contains(':') =>
        List[Input](
          ExitGame, Restart(Easy), Restart(Medium), Restart(Hard)
        ).find(value == _.encode)
      case text: String if text.length == 1 => Some(GuessSymbol(text.head))
      case text: String => NonEmptyList.fromList(text.toList).map(GuessWord)
      case _ => None
    }
  }
}