package ubjson

import org.scalacheck.Properties

object UbjsonProps extends Properties("ubjson"){

  property("couchDB4k")      = shouldParse("CouchDB4k.ubj")
  property("mediaContent")   = shouldParse("MediaContent.ubj")
  property("twitterContent") = shouldParse("TwitterTimeline.ubj")

  def shouldParse(f:String) = {
    val data = readFile("src/test/resources/ubjson/" + f)
    val result = UbjsonParsers.value(data)
    result.isSuccess
  }

  def readFile(f:String) = {
    import java.io._
    val file = new RandomAccessFile(f, "r")
    val b = Array.ofDim[Byte](file.length().toInt)
    file.read(b)
    file.close()
    b
  }
}
