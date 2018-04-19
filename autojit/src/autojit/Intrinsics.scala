package autojit

object Intrinsics {
  trait Foreach[T <: AnyRef]{
    val items: Seq[T]
  }
}
