package byteme

import org.scalacheck.{Properties, Prop, Arbitrary, Gen}
import Prop._
import Arbitrary.arbitrary
import java.util.Arrays

object ParserProps extends Properties("Parser"){
  import Parsers._

  property("byte") = forAll{ b:Byte =>
    byte(Array(b)).get == b
  }
  
  property("byte.unsigned") = forAll{ b:Byte =>
    byte.unsigned(Array(b)).get == (b & 0xFF)
  }
  
  property("~>!") = forAll{ (a:Byte, b:Byte) =>
    (byte ~>! byte)(Array(a, b)).get == b
  }
  
  property("~>!") = forAll{ (a:Byte, b:Byte) =>
    !(byte ~>! fail("X") | byte ~ byte)(Array(a, b)).isSuccess
  }
  
  property("~>!") = forAll{ (a:Byte, b:Byte) =>
    (fail("X") ~>! byte | byte ~ byte)(Array(a, b)).get == new ~(a, b)
  }

  property("*") = forAll{ array:Array[Byte] =>
    val result = (byte.*)(array).get.toArray
    Arrays.equals(result, array)
  }

  property("* <int>") = forAll{ (array:Array[Byte], b:Byte) =>
    val index = array.indexOf(b)
    (index >= 0) ==> {
      val result = (byte * index)(array).get
      result == array.take(index).toList
    }
  }
  
  property("~") = forAll{ (a:Byte, b:Byte) =>
    (byte ~ byte)(Array(a, b)).get == new ~(a, b) 
  }
  
  property("~>") = forAll{ (a:Byte, b:Byte) =>
    (byte ~> byte)(Array(a, b)).get == b
  }
  
  property("<~") = forAll{ (a:Byte, b:Byte) =>
    (byte <~ byte)(Array(a, b)).get == a
  }
  
  property("|") = forAll{ a:Byte =>
    (byte | fail("right"))(Array(a)).get == a
  }
  
  property("|") = forAll{ (a:Byte, b:Byte) =>
    (fail("left") | byte)(Array(a)).get == a
  }
  
  property("^^^") = forAll{ (a:Byte, b:String) =>
    (byte ^^^ b)(Array(a)).get == b
  }
  
  property("^^ (alias map)") = forAll{ data:Array[Byte] =>
    def f(b:Byte) = b.toString
    (byte ^^ f)(data) == (byte map f)(data)
  }
  
  property(">> (alias flatMap)") = forAll{ data:Array[Byte] =>
    def next(x:Byte) = byte.map(y => List(x, y))
    (byte >> next)(data) == (byte flatMap next)(data)
  }
  
  property("flatMap") = forAll{ (a:Byte, b:Byte) =>
    def next(x:Byte) = byte.map(y => List(x, y))
    byte.flatMap(next)(Array(a, b)).get == List(a, b)
  }
  
  property("map") = forAll { (a:Byte, b:Int) =>
    byte.map(_ + b)(Array(a)).get == a + b
  }
  
  property("filter") = forAll { a:Byte =>
    byte.filter(_ => true)(Array(a)).get == a
  }
  
  property("filter") = forAll { a:Byte =>
    !byte.filter(_ => false)(Array(a)).isSuccess
  }
  
  property("withFilter") = forAll{ (a:Array[Byte], b:Boolean) =>
    byte.withFilter(_ => b)(a) == byte.filter(_ => b)(a)
  }

  property("takeWhile") = forAll{ (array:Array[Byte], b:Byte) =>
    val result = byte.takeWhile(_ != b)(array).get
    val expected = array.takeWhile(_ != b).toList
    result == expected
  }
  
  property("where") = forAll{ (a:Byte, msg:String) =>
    byte.where(_ => true, msg)(Array(a)).get == a
  }
  
  property("where") = forAll{ (a:Byte, msg:String) =>
    val Failure(m, _) = byte.where(_ => false, msg)(Array(a))
    m == msg
  }
  
  property("bytes") = forAll(arbitrary[Array[Byte]], Gen.posNum[Int]) { (a, times) =>
    (times <= a.length) ==> Arrays.equals((bytes(times))(a).get, a.take(times))
  }
  
  property("until") = forAll { (a:Array[Byte], b:Byte) =>
    val index = a.indexOf(b)
    val result = byte.until(b)(a) 
    ((index == -1) ==> !result.isSuccess).label("failure") ||
    ((index != -1) ==> (result.get == a.take(index))).label("success")
  }
}
