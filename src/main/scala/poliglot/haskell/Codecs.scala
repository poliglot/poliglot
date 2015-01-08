package poliglot.haskell

import scodec.Codec

/**
 * Implements binary encoding format of Data.Binary.
 */
object Codecs {
  import scodec.codecs._

  implicit def sized[T](codec: Codec[T]) = variableSizeBytesLong(int, codec)

  implicit val int: Codec[Types.Int] = scodec.codecs.int64
  implicit val integer: Codec[Types.Integer] = new codecs.IntegerCodec()
  implicit val string: Codec[String] = sized(utf8)
  implicit val float = {
    ("a" | integer) ::
    ("b" | int)
  }.as[Types.Float]
  implicit val double: Codec[Types.Double] = float
  implicit val bool: Codec[Types.Bool] = scodec.codecs.bool(8)

  implicit def list[T](item: Codec[T]): Codec[Types.List[T]] = new codecs.ListCodec[T](item)
  implicit def map[A, B](key: Codec[A], value: Codec[B]): Codec[Types.Map[A, B]] = new codecs.MapCodec[A, B](key, value)
}
