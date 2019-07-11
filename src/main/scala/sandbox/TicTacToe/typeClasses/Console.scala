package sandbox.TicTacToe.typeClasses


  trait Console[F[_]] {
    def readLine: F[String]
    def printLine(text: String): F[Unit]
  }

  object Console {
    def apply[F[_]](implicit ev: Console[F]): Console[F] = ev
  }