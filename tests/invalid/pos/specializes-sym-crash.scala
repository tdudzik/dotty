// This relies on the naming of the transformed classes which will have to change in the new stdlib.
import scala.collection.*

trait Foo[+A,
                     +Coll,
                     +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
extends Seq[A] with SeqLike[A, This] with IterableView[A, Coll] with IterableViewLike[A, Coll, This] {
self =>

  trait TransformedFoo[+B] extends SeqView[B, Coll] with super.Transformed[B] {
    def length: Int
    def apply(idx: Int): B
    override def toString = viewToString
  }
  trait Reversed extends TransformedFoo[A] {
    override def iterator: Iterator[A] = createReversedIterator
    def length: Int = self.length
    def apply(idx: Int): A = self.apply(length - 1 - idx)
    final override protected[this] def viewIdentifier = "R"

    private def createReversedIterator = {
      var lst = List[A]()
      for (elem <- self) lst ::= elem
      lst.iterator
    }
  }
}
