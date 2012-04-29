package byteme

import org.scalacheck.{Gen, Properties, Arbitrary, Prop}
import Prop._
import Arbitrary.arbitrary

abstract class InputProps[A <: Input : Manifest] extends Properties(manifest[A].erasure.getSimpleName){
  def gen:Gen[A]

  property("first / atEnd") = forAll(gen){ input =>
    input.atEnd ==> (input.first throws classOf[IndexOutOfBoundsException])
  }

  property("first / !atEnd") = forAll(gen){ input =>
    !input.atEnd ==> {
      input.first
      true
    }
  }

  property("rest / atEnd") = forAll(gen){ input =>
    input.atEnd ==> (input.rest throws classOf[IndexOutOfBoundsException])
  }

  property("rest / !atEnd") = forAll(gen){ input =>
    !input.atEnd ==> {
      input.rest
      true
    }
  }

  property("length / atEnd") = forAll(gen){ input =>
    input.atEnd ==> (input.length == 0)
  }

  property("length / !atEnd") = forAll(gen){ input =>
    !input.atEnd ==> (input.length > 0)
  }

  property("view") = forAll(gen){ input =>
    input.view(0,0).atEnd
  }

  property("view") = forAll(gen){ input =>
    input.view(0, input.length).length == input.length
  }

  property("view") = forAll(gen){ input =>
    input.view(input.length, input.length).atEnd
  }

  property("toArray") = forAll(gen){ input =>
    val array = input.toArray
    array.length == input.length
  }

  property("toArray") = forAll(gen){ input =>
    def c(l:List[Byte], i:Input):Boolean = l match {
      case Nil => i.atEnd
      case h :: tail => h == i.first && c(tail, i.rest)
    }
    c(input.toArray.toList, input)
  }
}

object ArrayInputViewProps extends InputProps[ArrayInputView]{
  lazy val gen = for{
    array <- arbitrary[Array[Byte]]
    start <- Gen.chooseNum(0, array.length)
    end   <- Gen.chooseNum(start, array.length)
  } yield ArrayInputView(array, start, end)
}
