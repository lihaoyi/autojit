package autojit

object Intrinsics {
  trait Foreach[T <: AnyRef]{
    val items: Seq[T]
  }
  trait Mapped[T <: AnyRef, V <: AnyRef]{
    val items: IndexedSeq[T]
    def wrap(t: Array[V]): V
  }
}
