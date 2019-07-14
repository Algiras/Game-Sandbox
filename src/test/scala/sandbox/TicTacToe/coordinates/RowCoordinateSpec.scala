package sandbox.TicTacToe.coordinates

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import sandbox.TicTacToe.typeClasses.encoderDecoderParity

class RowCoordinateSpec extends Specification {
  "Encoding/Decoder" >> {
    encoderDecoderParity[RowCoordinate](NonEmptyList.fromListUnsafe(
      List(Row1, Row2, Row3).zip(
      List("1",  "2",  "3"))
    ))
  }
}
