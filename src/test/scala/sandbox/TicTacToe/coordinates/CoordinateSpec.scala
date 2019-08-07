package sandbox.TicTacToe.coordinates

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import sandbox.typeClasses._
import CoordinateSpec._

class CoordinateSpec extends Specification{
  "Encoding/Decoder" >> {
    encoderDecoderParity[Coordinate](NonEmptyList.fromListUnsafe(
      List( A1,   A2,   A3,   B1,   B2,   B3,   C1,   C2,   C3).zip(
      List("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"))
    ))
  }

  "all combinations" >> {
    combinations must contain(exactly(A1, A2, A3, B1, B2, B3, C1, C2, C3))
  }
}

object CoordinateSpec {
  val A1 = Coordinate(Col1, Row1)
  val A2 = Coordinate(Col1, Row2)
  val A3 = Coordinate(Col1, Row3)
  val B1 = Coordinate(Col2, Row1)
  val B2 = Coordinate(Col2, Row2)
  val B3 = Coordinate(Col2, Row3)
  val C1 = Coordinate(Col3, Row1)
  val C2 = Coordinate(Col3, Row2)
  val C3 = Coordinate(Col3, Row3)
}
