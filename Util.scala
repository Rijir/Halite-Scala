import scala.collection

object Util {
  def argCompare [A, B] (xs: Iterable[A], f: A => B, comp: (B, B) => Boolean): Option[A] = {
    def helper: (Iterable[A], Option[A], Option[B]) => Option[A] = {
      case ((x::xs), None, None) => helper(xs, Some(x), Some(f(x)))
      case ((x::xs), Some(xCur), Some(fxCur)) =>
        if (comp(f(x), fxCur))
          helper(xs, Some(x), Some(f(x)))
        else
          helper(xs, Some(xCur), Some(fxCur))
      case (List(), xCur, _) => xCur
      case (xs, xCur, fxCur) => throw new Exception(
        "xs = " + xs.toString
          + ", xCur = " + xCur.toString
          + ", fxCur = " + fxCur.toString)
    }
    helper(xs, None, None)
  }

  def argMin [A, B <: Ordered[B]] (xs: Iterable[A], f: A => B): Option[A] = {
    argCompare(xs, f, (a1: B, a2: B) => a1 < a2)
  }
}
