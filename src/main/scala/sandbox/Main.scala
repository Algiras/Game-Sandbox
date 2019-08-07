package sandbox

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import com.softwaremill.sttp.quick.{sttp, _}
import sandbox.Hangman.WordService
import sandbox.typeClasses.Console

import scala.util.Random

object Main extends IOApp {

  case object ApiWordService extends WordService[IO] {
    override def getWord: IO[NonEmptyList[Char]] = for {
      wordApiResponse <- IO(sttp.get(uri"https://raw.githubusercontent.com/dwyl/english-words/master/words.txt").send())
      words <- (wordApiResponse.body match {
        case Right(value) => IO.pure(value)
        case Left(error) => IO.raiseError(new RuntimeException(error))
      }).map(_.split('\n'))
      number <- IO(Random.nextInt(words.length - 1))
      randomWord = NonEmptyList.fromList(words(number).toList)
      word <- if(randomWord.isDefined) IO(randomWord.get) else IO.raiseError(new RuntimeException("Word API returning weird values"))
    } yield  word
  }

  implicit val ioInstance: Console[IO] = new Console[IO] {
    import scala.io.StdIn

    override def readLine: IO[String] = IO(StdIn.readLine)
    override def printLine(text: String): IO[Unit] = IO(println(text))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val ticTacToe = TicTacToe.run[IO]
    val hangman = Hangman.run[IO] _

    for {
      _ <- ioInstance.printLine("Available options: ticTacToe, hangMan")
      name <- ioInstance.readLine
      exit <- name match {
        case "ticTacToe" => ticTacToe
        case "hangMan" => hangman(ApiWordService)
        case _ => IO(ExitCode.Success)
      }
    } yield exit
  }
}
