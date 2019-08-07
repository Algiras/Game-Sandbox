package sandbox.Hangman

final case class Lives(lives: Int) {
  def sub: Option[Lives] = if(lives - 1 > 0) {
    Some(Lives(lives - 1))
  } else None
}

object Lives {
  val MAX = 10
  val MIN = 1

   val max: Lives = Lives(MAX)
   val min: Lives = Lives(MIN)
   val avg: Lives = Lives((MAX + MIN) / 2)
}
