package sandbox.TicTacToe.typeClasses

trait Decoder[T] {
  def decode(value: String): Option[T]
}

object Decoder {
  def apply[T](implicit ev: Decoder[T]): Decoder[T] = ev

  implicit class DecoderClass[T: Decoder](value: String) {
    def decode: Option[T] = Decoder[T].decode(value)
  }
}