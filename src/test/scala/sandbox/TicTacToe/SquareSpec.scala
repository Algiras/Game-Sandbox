package sandbox.TicTacToe

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import sandbox.TicTacToe.Square._
import sandbox.typeClasses.encoderDecoderParity

class SquareSpec extends Specification {
  "Encoding/Decoder" >> {
    encoderDecoderParity[Square](NonEmptyList.fromListUnsafe(
      List(X, O).zip(
      List(X, O).map(Square.toSymbol(_).toString))
    ))
  }

  "Encoding/Decoder Option" >> {
    val empty = Square.empty.toString

    encoderDecoderParity[Option[Square]](NonEmptyList.fromListUnsafe(
      List(None,        Option(X),  Option(O)).zip(
           empty +: List(X,         O        ).map(Square.toSymbol(_).toString))
    ))
  }

  "opposite" >> {
    opposite(X) must_=== O
    opposite(O) must_=== X
  }
}
