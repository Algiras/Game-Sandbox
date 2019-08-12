package sandbox.Hangman

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import sandbox.Hangman.GameState._
import sandbox.typeClasses.Encoder
import cats.data.{NonEmptyList, NonEmptySet}
import cats.instances.char._
import sandbox.Hangman.Lives.Lives

class StateSpec extends Specification with ScalaCheck {
  val emptyGame: String = emptyGameScene.map(_.mkString("")).mkString("\n")
  def wordFromString(value: String): NonEmptyList[Char] = NonEmptyList.fromListUnsafe(value.toList)

  "State" should {
    "encode" >> {
      "Win" >> {
        val word = "magic"
        Encoder[GameState].encode(Win(wordFromString(word))) must beMatching(s".*$word.*")
      }

      "Lose" >> {
        val word = "magic"
        val guesses = List('a', 'g').foldLeft(Guesses('m')){ case (gs, s) => gs.copy(letters = gs.letters.map(_.add(s)))}

        Encoder[GameState].encode(Lose(wordFromString(word), guesses)) must beMatching(s".*$word.*a, g, m.*")
      }

      "Game" >> {
        "with choices" >> {
          val choices = Guesses(
            Some(NonEmptySet.of('m', 'a', 'g', 'd')),
            Some(NonEmptySet.of(NonEmptyList.fromListUnsafe("word".toList)))
          )

          Encoder[GameState].encode(Game(Lives.max, wordFromString("magic"), choices)).split('\n') must_=== emptyGame.split('\n') ++ Array(
            "word: m a g _ _",
            "choices: a, d, g, m, word",
            s"lives: ${Lives.MAX}"
          )
        }

        "with no choices" >> {
          Encoder[GameState].encode(Game(Lives.max, wordFromString("magic"), Guesses.empty)).split('\n') must_=== emptyGame.split('\n') ++ Array(
            "word: _ _ _ _ _",
            "choices: No guesses yet",
            s"lives: ${Lives.MAX}"
          )
        }

        "Lives" >> {
          "show hangman" >> {
            def buildStr(value: String) = {
              val lines = value.stripMargin('|').split("\n").tail
              val maxLength = lines.map(_.length).max

              lines.map(_.padTo(maxLength, " ").mkString("")).mkString("\n")
            }
            def subtractLives(subtract: Int): Lives =
              Range(1, subtract + 1).foldLeft(Option(Lives.max))((lf, _) => lf.flatMap(Lives.sub)).get

            (Encoder[Lives].encode(subtractLives(1)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(2)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(3)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(4)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(5)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(6)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(7)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(8)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||       |
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(subtractLives(9)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||       |
                  ||      /
                  ||
                  ||_________"""))
          }
        }
      }
    }
  }
}