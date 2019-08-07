package sandbox.TicTacToe

import cats.data.StateT
import cats.effect.ExitCode
import cats.instances.either._
import cats.syntax.either._
import org.specs2.mutable.Specification
import sandbox.TicTacToe.coordinates.CoordinateSpec._
import sandbox.TicTacToe.game.GameSpec.buildGameUnsafe
import sandbox.TicTacToe.game.{Game, Move}
import sandbox.typeClasses.Console

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
    run[TestState].run(TestCase(
      Seq("X", "A1", "A1", "B1", "A2", "B2", "A3"),
      Seq(Text.chooseText(X))
        ++ nextMoveDisplay(X)
        ++ nextMoveDisplay(O, Move(X, A1))
        ++ Seq(Text.invalidChoice)
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, B1))
        ++ nextMoveDisplay(O, Move(X, A1), Move(O, B1), Move(X, A2))
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, B1), Move(X, A2), Move(O, B2))
        ++ Seq(Text.winner(X)))
    ) must beRight(completedProgram)
  }

  "Playing a draw game" >> {
    run[TestState].run(TestCase(
      Seq("X", "A1", "A2", "B1", "B2", "C2", "C1", "A3", "B3", "C3"),
      Seq(Text.chooseText(X))
        ++ nextMoveDisplay(X)
        ++ nextMoveDisplay(O, Move(X, A1))
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, A2))
        ++ nextMoveDisplay(O, Move(X, A1), Move(O, A2), Move(X, B1))
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, A2), Move(X, B1), Move(O, B2))
        ++ nextMoveDisplay(O, Move(X, A1), Move(O, A2), Move(X, B1), Move(O, B2), Move(X, C2))
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, A2), Move(X, B1), Move(O, B2), Move(X, C2), Move(O, C1))
        ++ nextMoveDisplay(O, Move(X, A1), Move(O, A2), Move(X, B1), Move(O, B2), Move(X, C2), Move(O, C1), Move(X, A3))
        ++ nextMoveDisplay(X, Move(X, A1), Move(O, A2), Move(X, B1), Move(O, B2), Move(X, C2), Move(O, C1), Move(X, A3), Move(O, B3))
        ++ Seq(Text.draw))
    ) must beRight(completedProgram)
  }

  def gameMoves(moves: Seq[Move]): Option[Game] = {
    moves.foldLeft(Option(game.empty)) { case (cGame, move) => cGame.flatMap(game.move(_, move)) }
  }

  def nextMoveDisplay(square: Square, moves: Move*) = Seq(
    displayGameOnConsole(gameMoves(moves).get),
    Text.nextMove(square),
    Text.validInputs
  )

  val completedProgram = (TestCase(Seq.empty, Seq.empty), ExitCode.Success)

  case class TestCase(inputs: Seq[String], outputs: Seq[String])

  type TestState[A] = StateT[Either[String, ?], TestCase, A]

  def tesState[A](fn: TestCase => Either[String, (TestCase, A)]): StateT[Either[String, ?], TestCase, A] = StateT[Either[String, ?], TestCase, A](fn)

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
