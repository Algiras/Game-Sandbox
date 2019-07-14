package sandbox.TicTacToe

import cats.data.StateT
import cats.effect.ExitCode
import cats.instances.either._
import cats.syntax.either._
import org.specs2.mutable.Specification
import sandbox.TicTacToe.coordinates.CoordinatesSpec._
import sandbox.TicTacToe.game.GameSpec.buildGameUnsafe
import sandbox.TicTacToe.game.{Game, Move}
import sandbox.TicTacToe.typeClasses.Console

class TicTacToeSpec extends Specification {
  "Display Game in Console format" >> {
    displayGameOnConsole(
      buildGameUnsafe(
        "X O O",
        "O X X",
        ". . ."
      )
    ) must_=== Seq(
      "  A B C",
      "1 X O O",
      "2 O X X",
      "3 . . ."
    ).mkString("\n")
  }

  "Playing a winning game" >> {
    def move(square: Square, moves: Move*) = Seq(
      displayGameOnConsole(gameMoves(moves).get),
      Text.nextMove(square),
      Text.validInputs
    )

    run[TestState].run(TestCase(
      Seq("X", "A1", "A1", "B1", "A2", "B2", "A3"),
      Seq(Text.chooseText(X))
        ++ move(X)
        ++ move(O, Move(X, A1))
        ++ Seq(Text.invalidChoice)
        ++ move(X, Move(X, A1), Move(O, B1))
        ++ move(O, Move(X, A1), Move(O, B1), Move(X, A2))
        ++ move(X, Move(X, A1), Move(O, B1), Move(X, A2), Move(O, B2))
        ++ Seq(Text.winner(X))
      )
    ) must beRight((TestCase(Seq.empty, Seq.empty), ExitCode.Success))
  }

  "Playing a winning game" >> {
    def move(square: Square, moves: Move*) = Seq(
      displayGameOnConsole(gameMoves(moves).get),
      Text.nextMove(square),
      Text.validInputs
    )

    run[TestState].run(TestCase(
      Seq("X", "A1", "B1", "A2", "B2", "A3"),
      Seq(Text.chooseText(X))
        ++ move(X)
        ++ move(O, Move(X, A1))
        ++ move(X, Move(X, A1), Move(O, B1))
        ++ move(O, Move(X, A1), Move(O, B1), Move(X, A2))
        ++ move(X, Move(X, A1), Move(O, B1), Move(X, A2), Move(O, B2))
        ++ Seq(Text.winner(X))
    )
    ) must beRight((TestCase(Seq.empty, Seq.empty), ExitCode.Success))
  }

  def gameMoves(moves: Seq[Move]): Option[Game] = {
    moves.foldLeft(Option(game.empty)){ case (cGame, move) => cGame.flatMap(_ + move)}
  }

  case class TestCase(inputs: Seq[String], outputs: Seq[String])

  type TestState[A] = StateT[Either[String, ?], TestCase, A]

  def tesState[A](fn: TestCase => Either[String, (TestCase, A)]) = StateT[Either[String, ?], TestCase, A](fn)

  implicit val stateInstance: Console[TestState] = new Console[TestState] {
    override def readLine: TestState[String] = tesState(state => {
      state.inputs.headOption.toRight("No matching input left").flatMap(firstInput =>
        (TestCase(state.inputs.tail, state.outputs), firstInput).asRight
      )
    })

    override def printLine(text: String): TestState[Unit] = tesState(state => {
      state.outputs.headOption.toRight("No matching output left").flatMap(firstOutput => {
        if (firstOutput == text) {
          (TestCase(state.inputs, state.outputs.tail), ()).asRight
        } else {
          s"Output is not valid: expected '$firstOutput' actual '$text'".asLeft
        }
      })
    })
  }
}
