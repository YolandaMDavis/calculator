package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      (b() * b()) - (4 * a() * c() )
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      import scala.math.sqrt
      if(a() == 0 || delta() < 0)
        Set()
      else if (((-b() + sqrt(delta())) / (2 * a())) == ((-b() - sqrt(delta())) / (2 * a()))) {
        Set(((-b() + sqrt(delta())) / (2 * a())))
      }else{
        Set(((-b() + sqrt(delta())) / (2 * a())), ((-b() - sqrt(delta())) / (2 * a())))
      }
    }
  }
}
