package sandbox.TicTacToe.typeClasses

trait Encoder[T] {
  def encode(value: T): String
}

object Encoder {
  def apply[T](implicit ev: Encoder[T]): Encoder[T] = ev

  implicit class EncoderClass[T: Encoder](value: T) {
    def encode: String = Encoder[T].encode(value)
  }
}
