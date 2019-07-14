package sandbox.TicTacToe.game

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments
import sandbox.TicTacToe.coordinates.CoordinatesSpec._
import sandbox.TicTacToe.typeClasses.{Decoder, encoderDecoderParity}
import sandbox.TicTacToe.{O, Square, X, game}
import GameSpec._

class GameSpec extends Specification {
  "Encoding/Decoder" >> {
    encoderDecoderParity[Game](NonEmptyList.fromListUnsafe(
      List(Game(
        Row(Some(X), Some(X), Some(X)),
        Row(None,    None,    None   ),
        Row(Some(O), Some(O), Some(O)))).zip(List(
        "X X X\n" ++
        ". . .\n" ++
        "O O O"
      ))
    ))
  }

  "Making a move" >> {
    game.empty + Move(X, A1) must beSome(buildGameUnsafe(
      "X . .",
      ". . .",
      ". . ."
    ))

    game.empty + Move(X, B1) must beSome(buildGameUnsafe(
      ". X .",
      ". . .",
      ". . ."
    ))

    game.empty + Move(X, C1) must beSome(buildGameUnsafe(
      ". . X",
      ". . .",
      ". . ."
    ))

    // ---------------------------------------

    game.empty + Move(X, A2) must beSome(buildGameUnsafe(
      ". . .",
      "X . .",
      ". . ."
    ))

    game.empty + Move(X, B2) must beSome(buildGameUnsafe(
      ". . .",
      ". X .",
      ". . ."
    ))

    game.empty + Move(X, C2) must beSome(buildGameUnsafe(
      ". . .",
      ". . X",
      ". . ."
    ))

    // ---------------------------------------

    game.empty + Move(X, A3) must beSome(buildGameUnsafe(
      ". . .",
      ". . .",
      "X . ."
    ))

    game.empty + Move(X, B3) must beSome(buildGameUnsafe(
      ". . .",
      ". . .",
      ". X ."
    ))

    game.empty + Move(X, C3) must beSome(buildGameUnsafe(
      ". . .",
      ". . .",
      ". . X"
    ))
  }

  "check if anyone win" >> {
    val wins: Seq[(Game, Option[Square])] = Seq(
      (buildGameUnsafe(
        "O . O",
        "X X O",
        "X O X"
      ), None),
      (buildGameUnsafe(
        "O O O",
        "X X .",
        "X O X"
      ), Some(O)),
      (buildGameUnsafe(
        "O . O",
        "X X X",
        "O O X"
      ), Some(X)),
      (buildGameUnsafe(
        "O . O",
        "X O O",
        "X X X"
      ), Some(X)),
      (buildGameUnsafe(
        "X . O",
        "X X O",
        "X O O"
      ), Some(X)),
      (buildGameUnsafe(
        "O X .",
        "X X O",
        "O X X"
      ), Some(X)),
      (buildGameUnsafe(
        "O . O",
        "X X O",
        "X X O"
      ), Some(O)),
      (buildGameUnsafe(
        "O . X",
        "X X O",
        "X O O"
      ), Some(X)),
      (buildGameUnsafe(
        "X . O",
        "X X O",
        "O O X"
      ), Some(X)),
    )

    Fragments.foreach(wins){
      case (game, winner) => game.winner must_=== winner
    }
  }


  "tell if board is full" >> {
    val currentGame = buildGameUnsafe(
      "O . O",
      "X X O",
      "X O X"
    )

    currentGame.full must beFalse

    (currentGame + Move(X, B1)).map(_.full) must beSome(true)
  }

  "return None if game state after a move is not valid" >> {
    buildGameUnsafe(
      ". . .",
      ". . .",
      ". . X"
    ) + Move(X, C3) must beNone
  }
}

object GameSpec {
  def buildGameUnsafe(row1: String, row2: String, row3: String): Game = Decoder[Game].decode(Seq(
    row1,
    row2,
    row3
  ).mkString("\n")).get
}
