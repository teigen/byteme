package ubjson

import org.scalacheck.{Properties, Prop}
import Prop._
import java.util.Arrays

object UbjsonProps extends Properties("ubjson"){

  property("couchDB4k")      = check("CouchDB4k.ubj")
  
  property("mediaContent")   = check("MediaContent.ubj")
  
  property("twitterContent") = check("TwitterTimeline.ubj")
  
  def check(f:String) = {
    val data = readFile(f)
    lazy val result = UbjsonParsers.value(data)
    lazy val unpickled = UbjsonPicklers.value.unpickle(data)    
    
    result.isSuccess.label("parser") &&
    Arrays.equals(data, UbjsonOutput.value(result.get).toArray).label("output") &&
    (unpickled.get == result.get).label("unpickle") &&
    Arrays.equals(data, UbjsonPicklers.value.pickle(unpickled.get).toArray).label("pickle")
  }

  def readFile(f:String) = {
    import java.io._
    val file = new RandomAccessFile("src/test/resources/ubjson/"+f, "r")
    val b = Array.ofDim[Byte](file.length().toInt)
    file.read(b)
    file.close()
    b
  }
}
