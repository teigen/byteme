package bson

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Properties, Gen}

import org.bson.{BSON, BasicBSONEncoder, BSONObject, BasicBSONObject}
import org.bson.types.{Symbol => BsonSymbol, MaxKey => BsonMaxKey, MinKey => BsonMinKey, _}
import org.bson.io.BasicOutputBuffer

import java.util.Date
import java.util.regex.Pattern

object BsonProps extends Properties("bson"){

  val cStr = {
    val cChar = arbitrary[Char].suchThat(_ != Char.MinValue)
    Gen.listOf(cChar).map(_.mkString)
  }

  val objId = arbitrary[Date].map(d => new ObjectId(d))

  val regexFlag = Gen.oneOf(
    Pattern.CANON_EQ,
    Pattern.UNIX_LINES,
    256, // GLOBAL_FLAG
    Pattern.CASE_INSENSITIVE,
    Pattern.MULTILINE,
    Pattern.DOTALL,
    Pattern.LITERAL,
    Pattern.UNICODE_CASE,
    Pattern.COMMENTS)

  val regexFlags = for{
    n <- Gen.choose(0, 9)
    flags <- Gen.containerOfN[Set, Int](n, regexFlag)
  } yield flags.fold(0)(_ | _)

  val pattern:Gen[Pattern] = for {
    s <- Gen.alphaStr
    f <- regexFlags
  } yield Pattern.compile(s, f)

  implicit val arbSymbol = Arbitrary{ arbitrary[String].map(Symbol(_)) }

  property("\\x01 double") = forAll(cStr, arbitrary[Double]){ (name, value) =>
    check(name, value, MDouble(value))
  }

  property("\\x02 string") = forAll(cStr, arbitrary[String]){ (name, value) =>
    check(name, value, MString(value))
  }

  property("\\x03 document") = forAll(cStr, cStr, arbitrary[String]){ (name, nested, value) =>
    check(name, new BasicBSONObject(nested, value), Document(List(Element(nested, MString(value)))))
  }

  property("\\x04 array") = forAll(cStr, arbitrary[List[String]]){ (name, value) =>
    import collection.JavaConverters._
    check(name, value.asJava, MArray(value.map(MString(_))))
  }

  property("\\x05 binary") = forAll(cStr, arbitrary[Array[Byte]]){ (name, value) =>
    check(name, value, MBinary(Subtype.Generic, value))
  }

  property("\\x07 objectid") = forAll(cStr, objId){ (name, value) =>
    check(name, value, MObjectId(value.toByteArray))
  }

  property("\\x08 boolean") = forAll(cStr, arbitrary[Boolean]){ (name, value) =>
    check(name, value, MBoolean(value))
  }

  property("\\x09 int64") = forAll(cStr, arbitrary[Long]){ (name, value) =>
    check(name, value, MLong(value))
  }

  property("\\x0A null") = forAll(cStr){ name =>
    check(name, null, MNull)
  }

  property("\\x0B regexp") = forAll(cStr, pattern){ (name, value) =>
    check(name, value, MRegexp(value.toString, BSON.regexFlags(value.flags())))
  }

  property("\\x0D javascript") = forAll(cStr, arbitrary[String]){ (name, value) =>
    check(name, new Code(value), MCode(value))
  }

  property("\\x0E symbol") = forAll(cStr, arbitrary[Symbol]){ (name, value) =>
    check(name, new BsonSymbol(value.name), MSymbol(value))
  }

  property("\\x0F javascript code w/scope") = forAll(cStr, arbitrary[String], cStr, arbitrary[Double]){ (name, value, nested, nval) =>
    check(name, new CodeWScope(value, new BasicBSONObject(nested, nval)), MCodeWithScope(value, Document(List(Element(nested, MDouble(nval))))))
  }

  property("\\x10 32 bit integer") = forAll(cStr, arbitrary[Int]){ (name, value) =>
    check(name, value, MInt(value))
  }

  property("\\x11 timestamp") = forAll(cStr, arbitrary[Date], Gen.posNum[Int]){ (name, d, inc) =>
    val time = (d.getTime / 1000).toInt
    check(name, new BSONTimestamp(time, inc), MTimestamp(time, inc))
  }

  property("\\x12 64 bit integer") = forAll(cStr, arbitrary[Long]){ (name, value) =>
    check(name, value, MLong(value))
  }

  property("\\xFF Min key") = forAll(cStr){ name =>
    check(name, new BsonMinKey(), MinKey)
  }

  property("\\x7F MaxKey") = forAll(cStr){ name =>
    check(name, new BsonMaxKey(), MaxKey)
  }

  def check(name:String, any:Any, value:Value) = {
    val encoded = encode(new BasicBSONObject(name, any))
    val document = BsonParsers.document(encoded).get
    Document(List(Element(name, value))) == document
  }

  def encode(bson:BSONObject):Array[Byte] = {
    val encoder = new BasicBSONEncoder
    val out = new BasicOutputBuffer
    encoder.set(out)
    encoder.putObject(bson)
    out.toByteArray
  }
}
