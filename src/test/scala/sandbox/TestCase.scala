package sandbox

import cats.data.StateT
import cats.effect.ExitCode
import cats.instances.either._
import cats.syntax.either._
import sandbox.typeClasses.Console

case class TestCase(inputs: Seq[String], outputs: Seq[String])

object TestCase {
  val completedProgram: (TestCase, ExitCode) = (TestCase(Seq.empty, Seq.empty), ExitCode.Success)

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
