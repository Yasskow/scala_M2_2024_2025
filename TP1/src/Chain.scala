package fr.uge.td.scala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

class Chain{

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => list
      case Cons(_, tail) => tail
    }
  }

  def get[A](list: List[A], index: Int): Either[String, A] = {
    list match {
      case Nil => Left("Pas de place")
      case Cons(head, tail) => if (index == 0) Right(head) else get(tail, index-1)
    }
  }
}

object Chain{
  def main(args: Array[String]): Unit = {
    val chain = new Chain()
    val cons = Cons(1,Cons(2, Cons(3, Nil)))
    println(chain.tail(cons))
    println(chain.get(cons, 1))
  }
}