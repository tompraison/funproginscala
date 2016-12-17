package chapter3

/**
  * Created by tpraison on 12/13/16.
  */
object Exercise {

  def main(args: Array[String]): Unit = {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    // Exercise 3.1
    println(x) // The result must be 3

    // Exercise 3.2
    println(List.tail(List(1,2,3,4)))

    // Exercise 3.3
    println(List.setHead(List(1,2,3,4),10))

    // Exercise 3.4
    println(List.drop(List(1, 2, 3, 4), 2))
    println(List.drop(List(1, 2, 3, 4), 0))
    println(List.drop(List(1, 2, 3, 4), 5))

    // Exercise 3.5
    println(List.dropWhile(List(1, 2, 3, 4), (x:Int) => x < 3))

    // Exercise 3.6
    println(List.init(List(1)))
    println(List.init(List(1,2)))
    println(List.init(List(1, 2, 3, 4)))
  }
}

sealed trait List[+T]

object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def tail[T](l: List[T]) = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(h, t) => t
  }

  def setHead[T](l: List[T], ele: T) = l match {
    case Nil => Cons(ele, Nil)
    case Cons(h, t) => Cons(ele, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if (n == 0) => l
    case Cons(_, t) => drop(t, n - 1)
  }

  def dropWhile[T](as: List[T], f: T => Boolean):List[T] = {
    as match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => as
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}