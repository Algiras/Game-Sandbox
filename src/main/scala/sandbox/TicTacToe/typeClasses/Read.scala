package sandbox.TicTacToe.typeClasses

trait Read[T] {
  def read(value: String): Option[T]
}

object Read {
  def apply[T](implicit ev: Read[T]): Read[T] = ev
}