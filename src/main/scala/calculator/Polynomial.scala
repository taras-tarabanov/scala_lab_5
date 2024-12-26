package calculator

import math._

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    //По формулі:  Δ = b² - 4ac
    Signal(pow(b(), 2) - 4 * a() * c())



  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    // По формулі:  (-b ± √Δ) / 2a
    Signal(computeDelta(a, b, c)() match {
      case x if x < 0 => Set()
      case x =>
        val sqrtDelta = sqrt(x)
        val s1 = (-b() + sqrtDelta) / (2*a())
        val s2 = (-b() - sqrtDelta) / (2*a())
        Set(s1, s2)
    })