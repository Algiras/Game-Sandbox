package sandbox.Hangman

object Lives {
  type Lives = Int

  val MAX = 10
  val MIN = 1

  def sub(lives: Lives): Option[Lives] = if(lives - 1 > 0) {
    Some(lives - 1)
  } else None

   val max: Lives = MAX
   val min: Lives = MIN
   val avg: Lives = (MAX + MIN) / 2
}
