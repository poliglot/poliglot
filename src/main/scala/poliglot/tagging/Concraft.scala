package poliglot.tagging

import java.io.{DataInputStream, DataOutputStream}
import java.net.Socket

import scodec.Codec
import scodec.bits.BitVector
import poliglot.formats.AnnotatedToken
import poliglot.haskell

import scala.collection.mutable.ArrayBuffer

object Concraft {
  import scodec.codecs._

  case class Short(text: String)

  // Tag with marginal probabilities.
  case class Config(tagProbs: Boolean)

  case class Request(body: Short, conf: Config)
  case class MainRequest(request: Request)
  case class Response(sentences: List[Sentence])
  case class MainResponse(response: Response)

  trait Space
  object Space {
    case object None extends Space
    case object Space extends Space
    case object NewLine extends Space
  }

  case class Word(orth: String, space: Space, known: Boolean)
  case class Interp(base: String, tag: String)
  case class Segment(word: Word, interps: Map[Interp, haskell.Types.Double]) {
    def interp: Interp = interps.find(_._2 != haskell.Types.Float(0, 0)).get._1
  }
  case class Sentence(segments: List[Segment])

  implicit val shortCodec = {
    ("type" | constant(0)) :~>:
    ("text" | haskell.Codecs.string)
  }.as[Short]

  implicit val configCodec = {
    "tagProbs" | haskell.Codecs.bool
  }.as[Config]

  implicit val requestCodec = {
    ("body" | shortCodec) ::
    ("conf" | configCodec)
  }.as[Request]

  implicit val mainRequestCodec = {
    "request" | haskell.Codecs.sized(requestCodec)
  }.as[MainRequest]

  implicit val spaceCodec: Codec[Space] = mappedEnum(uint8,
    Space.None -> 1,
    Space.Space -> 2,
    Space.NewLine -> 3)

  implicit val wordCodec = {
    ("orth"  | haskell.Codecs.string) ::
    ("space" | spaceCodec) ::
    ("known" | bool(8))
  }.as[Word]

  implicit val interpCodec = {
    ("base" | haskell.Codecs.string) ::
    ("tag" | haskell.Codecs.string)
  }.as[Interp]

  implicit val segmentCodec = {
    ("word" | wordCodec) ::
    ("interps" | haskell.Codecs.map(interpCodec, haskell.Codecs.double))
  }.as[Segment]

  implicit val sentenceCodec = {
    "segments" | haskell.Codecs.list(segmentCodec)
  }.as[Sentence]

  implicit val responseCodec = {
    "sentences" | haskell.Codecs.list(sentenceCodec)
  }.as[Response]

  implicit val mainResponseCodec = {
    "response" | haskell.Codecs.sized(responseCodec)
  }.as[MainResponse]

  // TODO non-blocking sockets and return Future[Array[Byte]]
  def sendRequest(addr: String, port: Int, sentence: String): Array[Byte] = {
    val socket = new Socket(addr, port)
    val out = new DataOutputStream(socket.getOutputStream)
    val in = new DataInputStream(socket.getInputStream)

    val request = Codec.encodeValid(MainRequest(Request(Short(sentence), Config(tagProbs = false))))
    out.write(request.toByteArray)
    out.flush()

    val data = new ArrayBuffer[Byte]()

    var stop = false
    while (!stop) {
      val bytes = new Array[Byte](1024)
      val read = in.read(bytes)
      data ++= bytes.slice(0, read)
      stop = read == -1
    }

    out.close()
    in.close()
    socket.close()

    data.toArray
  }

  def lookUp(data: Array[Byte]): List[Sentence] = {
    val decoded = Codec.decodeValue[MainResponse](BitVector(data))
    decoded.map(_.response.sentences).getOrElse(List.empty)
  }

  def lookUp(addr: String, sentence: String, port: Int = 10089): List[Sentence] =
    lookUp(sendRequest(addr, port, sentence))

  def tokens(sentences: List[Sentence]): List[AnnotatedToken] =
    sentences.flatMap(sentence =>
      sentence.segments.map(token =>
        AnnotatedToken(
          token.word.orth,
          token.interp.base,
          token.interp.tag.split(":"))
      )
    )
}