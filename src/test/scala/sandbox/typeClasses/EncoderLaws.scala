package sandbox.typeClasses

import org.specs2.matcher.MatchResult

object EncoderLaws {
  def reflectiveToDecoder[T: Encoder: Decoder](value: String): MatchResult[String] = Encoder[T].encode(Decoder[T].decode(value).get) must_=== value
}
