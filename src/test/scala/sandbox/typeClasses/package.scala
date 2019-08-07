package sandbox

import cats.data.NonEmptyList
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

package object typeClasses extends Specification {
  def encoderDecoderParity[T: Encoder: Decoder](values: NonEmptyList[(T, String)]) = {
    values.map(_._1).map(DecoderLaws.reflectiveToEncoder[T](_)).reduce[MatchResult[Option[T]]](_ and _) and
    values.map(_._2).map(EncoderLaws.reflectiveToDecoder[T](_)).reduce[MatchResult[String]](_ and _) and
    values.map{ case (left, right) => Encoder[T].encode(left) must_=== right }.reduce[MatchResult[String]](_ and _) and
    values.map{ case (left, right) => Decoder[T].decode(right) must beSome(left) }.reduce[MatchResult[Option[T]]](_ and _)
  }
}
