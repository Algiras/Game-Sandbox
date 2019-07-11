package sandbox

import TicTacToe._
import cats.Show
import org.specs2.mutable.Specification
import cats.syntax.show._
import org.specs2.specification.core.Fragments
import sandbox.TicTacToe.coordinates.{Col1, Col2, Col3, Coordinates, Row1, Row2, Row3}
import sandbox.TicTacToe.game.{Game, Move, Row}
import sandbox.TicTacToe.game
import sandbox.TicTacToe.game.Row._
import sandbox.TicTacToe.typeClasses.Read

class TicTacToeSpec extends Specification {
  "Read" >> {
    "reverse Show" >> {
      Read[Square].read(Show[Square].show(X)) must some(X)
      Read[Square].read(Show[Square].show(O)) must some(O)
      Read[Square].read(".") must beNone

      Read[Row].read("X O .") must beSome(Row(Some(X), Some(O), None))
      Read[Game].read(Seq(
        "X X .",
        "O O O",
        ". . X"
      ).mkString("\n")) must beSome(Game(
        Row(Some(X), Some(X), None   ),
        Row(Some(O), Some(O), Some(O)),
        Row(None,    None,    Some(X))
      ))
    }
  }

  "Show" >> {
    "show empty value as .">> {
      Show[Option[Square]].show(None) must_=== "."
      Show[Option[Square]].show(Some(X)) must_=== "X"
      Show[Option[Square]].show(Some(O)) must_=== "O"
    }

    "show row" >> {
      Row(Some(X), Some(X), Some(X)).show must_=== "X X X"
      Row(Some(O), Some(O), Some(O)).show must_=== "O O O"
      Row(None,    None,    None   ).show must_=== ". . ."
    }

    "show currentGame" >> {
      buildGame(
        "X X .",
        "O O O",
        ". . X"
      ).show must_=== Seq(
        "X X .",
        "O O O",
        ". . X"
      ).mkString("\n")

    }

    "show currentGame on console" >> {
      displayGameOnConsole(
        buildGame(
          "X X .",
          "O O O",
          ". . X"
        )
      ) must_=== Seq(
        "  A B C",
        "1 X X .",
        "2 O O O",
        "3 . . X"
      ).mkString("\n")
    }

  }

  "Coordinates" >> {
    "Read String coordinates" >> {
      Read[Coordinates].read("A1") must beSome(Coordinates(Col1, Row1))
      Read[Coordinates].read("B2") must beSome(Coordinates(Col2, Row2))
      Read[Coordinates].read("C3") must beSome(Coordinates(Col3, Row3))
    }
    "Show String coordinates" >> {
      Coordinates(Col1, Row1).show must_=== "A1"
      Coordinates(Col2, Row2).show must_=== "B2"
      Coordinates(Col3, Row3).show must_=== "C3"
    }
  }

  "Playing the currentGame" >> {
    "produce new currentGame state after getting state and move" >> {
      import coordinates._

      game.empty + Move(X, A1) must beSome(buildGame(
        "X . .",
        ". . .",
        ". . ."
      ))

      game.empty + Move(X, B1) must beSome(buildGame(
        ". X .",
        ". . .",
        ". . ."
      ))

      game.empty + Move(X, C1) must beSome(buildGame(
        ". . X",
        ". . .",
        ". . ."
      ))

      // ---------------------------------------

      game.empty + Move(X, A2) must beSome(buildGame(
        ". . .",
        "X . .",
        ". . ."
      ))

      game.empty + Move(X, B2) must beSome(buildGame(
        ". . .",
        ". X .",
        ". . ."
      ))

      game.empty + Move(X, C2) must beSome(buildGame(
        ". . .",
        ". . X",
        ". . ."
      ))

      // ---------------------------------------

      game.empty + Move(X, A3) must beSome(buildGame(
        ". . .",
        ". . .",
        "X . ."
      ))

      game.empty + Move(X, B3) must beSome(buildGame(
        ". . .",
        ". . .",
        ". X ."
      ))

      game.empty + Move(X, C3) must beSome(buildGame(
        ". . .",
        ". . .",
        ". . X"
      ))
    }

    "return None if currentGame state is not valid" >> {
      import coordinates._

      buildGame(
        ". . .",
        ". . .",
        ". . X"
      ) + Move(X, C3) must beNone
    }

    "tell if board is full" >> {
      import coordinates._

      val currentGame = buildGame(
        "O . O",
        "X X O",
        "X O X"
      )

      currentGame.full must beFalse

      (currentGame + Move(X, B1)).map(_.full) must beSome(true)
    }

    "check if anyone win" >> {
      val wins: Seq[(Game, Option[Square])] = Seq(
        (buildGame(
        "O . O",
        "X X O",
        "X O X"
        ), None),
        (buildGame(
          "O O O",
          "X X .",
          "X O X"
        ), Some(O)),
        (buildGame(
          "O . O",
          "X X X",
          "O O X"
        ), Some(X)),
        (buildGame(
          "O . O",
          "X O O",
          "X X X"
        ), Some(X)),
        (buildGame(
          "X . O",
          "X X O",
          "X O O"
        ), Some(X)),
        (buildGame(
          "O X .",
          "X X O",
          "O X X"
        ), Some(X)),
        (buildGame(
          "O . O",
          "X X O",
          "X X O"
        ), Some(O)),
        (buildGame(
          "O . X",
          "X X O",
          "X O O"
        ), Some(X)),
        (buildGame(
          "X . O",
          "X X O",
          "O O X"
        ), Some(X)),
      )

      Fragments.foreach(wins){
        case (game, winner) => game.winner must_=== winner
      }
    }
  }

  private def buildGame(row1: String, row2: String, row3: String): Game = Read[Game].read(Seq(
    row1,
    row2,
    row3
  ).mkString("\n")).get
}
