package chapter2

import scala.annotation.tailrec

/**
  * Created by tompraison on 12/11/16.
  */
object Exercise {

  // Exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(n1: Int, a: Int, b: Int): Int = {
      if (n1 <= 1) a else loop(n1 - 1, b, a + b)
    }
    require(n >= 0, "n must be greater than or equal to 0")
    loop(n, 0, 1)
  }


  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else (ordered(as(n - 1), as(n)) && loop(n + 1))
    }
    if (as.length <= 1) true else loop(1)
  }

  // Exercise 2.3
  def currying[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }
}
