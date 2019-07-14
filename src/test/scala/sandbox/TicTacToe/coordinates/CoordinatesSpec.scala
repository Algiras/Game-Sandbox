package sandbox.TicTacToe.coordinates

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import sandbox.TicTacToe.typeClasses._
import CoordinatesSpec._

class CoordinatesSpec extends Specification{
  "Encoding/Decoder" >> {
    encoderDecoderParity[Coordinates](NonEmptyList.fromListUnsafe(
      List( A1,   A2,   A3,   B1,   B2,   B3,   C1,   C2,   C3).zip(
      List("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"))
    ))
  }

  "all combinations" >> {
    combinations must contain(exactly(A1, A2, A3, B1, B2, B3, C1, C2, C3))
  }
}

object CoordinatesSpec {
  val A1 = Coordinates(Col1, Row1)
  val A2 = Coordinates(Col1, Row2)
  val A3 = Coordinates(Col1, Row3)
  val B1 = Coordinates(Col2, Row1)
  val B2 = Coordinates(Col2, Row2)
  val B3 = Coordinates(Col2, Row3)
  val C1 = Coordinates(Col3, Row1)
  val C2 = Coordinates(Col3, Row2)
  val C3 = Coordinates(Col3, Row3)
}
