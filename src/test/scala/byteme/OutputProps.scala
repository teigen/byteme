package byteme

import org.scalacheck.{Properties, Prop, Arbitrary, Gen}
import Prop._
import Arbitrary._
import java.util.Arrays

object OutputProps extends Properties("output"){
  property("byte") = forAll{ b:Byte =>
    Output.byte(b).length == 1
  }
  
  property("byte") = forAll{ b:Byte =>
    Arrays.equals(Output.byte(b).toArray, Array(b))
  }
  
  property("byte") = forAll{ (b:Byte, array:Array[Byte], i:Int) =>
    (i < array.length && i >= 0) ==> {
      Output.byte(b).write(array, i)
      array(i) == b
    }
  }
  
  property("byte") = forAll{ (b:Byte, array:Array[Byte], i:Int) =>
    (i >= array.length) ==> (Output.byte(b).write(array, i) throws classOf[IndexOutOfBoundsException])
  }
  
  property("byte.unsigned") = forAll { i:Int =>
    (i >= 0 && i < 256) ==> (Output.byte.unsigned(i).toArray(0) == i.asInstanceOf[Byte])
  }
  
  property("byte.unsigned") = forAll{ i:Int =>
    !(i >= 0 && i < 256) ==> (Output.byte.unsigned(i) throws classOf[RuntimeException]) 
  }
  
  property("byte.unsigned") = forAll{ i:Int =>
    (i >= 0 && i < 256) ==> (Output.byte.unsigned(i).length == 1)
  }
  
  property("array") = forAll{ a:Array[Byte] =>
    Output.array(a).length == a.length
  }
  
  property("array") = forAll{ a:Array[Byte] =>
    Arrays.equals(Output.array(a).toArray, a)
  }
  
  val nums = for {
    a <- arbitrary[Array[Byte]]
    b <- arbitrary[Array[Byte]]
    (max, min) = if(a.length < b.length) (b, a) else (a, b)
    num <- Gen.posNum[Int] if min.length + num > num && min.length + num <= max.length
  } yield (min, max, num)
  
  property("array") = forAll(nums){ case (a, b, i) =>
    Output.array(a).write(b, i)
    Arrays.equals(b.drop(i).take(a.length), a)
  }
  
  property("array") = forAll(nums){ case (b, a, i) =>
    Output.array(a).write(b, i) throws classOf[IndexOutOfBoundsException]
  }
  
  // TODO, gen as tree ?
  implicit val arbOutput = Arbitrary{
    val b = arbitrary[Byte].map(Output.byte)
    val a = arbitrary[Array[Byte]].map(Output.array)
    Gen.frequency(1 -> b, 3 -> a)
  }
  
  property("++") = forAll{ (a:Output, b:Output) =>
    (a ++ b).length == a.length + b.length
  }
  
  property("++") = forAll{ (a:Output, b:Output) =>    
    Arrays.equals((a ++ b).toArray, a.toArray ++ b.toArray)    
  }
  
  property("++") = forAll{ (a:Output, b:Output) =>
    val ab = a ++ b
    val array = Array.ofDim[Byte](ab.length)
    ab.write(array, 0)
    Arrays.equals(array, ab.toArray)
  }
}
