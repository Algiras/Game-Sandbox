package sandbox.Hangman

import cats.data.NonEmptyList
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import sandbox.Hangman.State._
import sandbox.typeClasses.Encoder

class StateSpec extends Specification with ScalaCheck {
  def wordFromString(value: String): NonEmptyList[Char] = NonEmptyList.fromListUnsafe(value.toList)

  "State" should {
    "encode" >> {
      "Win" >> {
        val word = "magic"
        Encoder[State].encode(Win(wordFromString(word))) must beMatching(s".*$word.*")
      }

      "Lose" >> {
        val word = "magic"
        val guesses = symbolChoices(NonEmptyList.of('m', 'a', 'g'))

        Encoder[State].encode(Lose(wordFromString(word), guesses)) must beMatching(s".*$word.*a, g, m.*")
      }

      "Game" >> {
        "with choices" >> {
          val choices = addWordGuess(
            symbolChoices(NonEmptyList.of('m', 'a', 'g', 'd')),
            NonEmptyList.fromListUnsafe("word".toList)
          )
          Encoder[State].encode(Game(Lives.max, wordFromString("magic"), Some(choices))).split('\n') must_=== emptyGame.split('\n') ++ Array(
            "word: m a g _ _",
            "choices: a, d, g, m, word",
            s"lives: ${Lives.MAX}"
          )
        }

        "with no choices" >> {
          Encoder[State].encode(Game(Lives.max, wordFromString("magic"), None)).split('\n') must_=== emptyGame.split('\n') ++ Array(
            "word: _ _ _ _ _",
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

            (Encoder[Lives].encode(Lives(Lives.MAX - 1)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 2)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 3)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 4)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 1)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||
                  ||
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 5)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||       |
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 6)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 7)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 8)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||       |
                  ||
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 9)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||       |
                  ||      /
                  ||
                  ||_________""")).and(Encoder[Lives].encode(Lives(Lives.MAX - 10)) must_===
              buildStr(
                """
                  ||_________
                  ||/      |
                  ||      (_)
                  ||      /|\
                  ||       |
                  ||      / \
                  ||
                  ||_________"""))
          }
        }
      }
    }
  }
}