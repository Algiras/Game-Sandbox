package sandbox

import cats.effect.{ExitCode, IO, IOApp}
import sandbox.TicTacToe.typeClasses.Console

object Main extends IOApp {

  implicit val ioInstance: Console[IO] = new Console[IO] {
    import scala.io.StdIn

    override def readLine: IO[String] = IO(StdIn.readLine)
    override def printLine(text: String): IO[Unit] = IO(println(text))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    TicTacToe.run[IO]
  }
}
