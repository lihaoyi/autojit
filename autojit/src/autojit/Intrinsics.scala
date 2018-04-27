package autojit

import java.lang.invoke.MethodHandle

object Intrinsics {
  trait Foreach[T <: AnyRef]{
    val items: Seq[T]
  }
  trait Mapped[T <: AnyRef, V <: AnyRef]{
    val items: IndexedSeq[T]
    def wrap(t: Array[V]): V
  }
  trait Lambda[E <: AnyRef]{
    val body: E
    def handle(): MethodHandle
  }
}
