package sandbox.Hangman

import sandbox.typeClasses.{Decoder, Encoder}
import sandbox.typeClasses.Encoder._

sealed trait Difficulty {
  def lives: Lives
}

case object Easy extends Difficulty {
  val lives: Lives = Lives.max
}

case object Medium extends Difficulty {
  val lives: Lives = Lives.avg
}

case object Hard extends Difficulty {
  val lives: Lives = Lives.min
}

object Difficulty {
  implicit val difficultyEncoder: Encoder[Difficulty] = {
    case Easy => "easy"
    case Medium => "medium"
    case Hard => "hard"
  }

  implicit val difficultyDecoder: Decoder[Difficulty] = (value: String) => List[Difficulty](Easy, Medium, Hard).find(_.encode == value)
}