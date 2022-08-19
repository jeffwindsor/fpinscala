package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def getOrElse[B>:A](default: B): B = this match
    case None => default
    case Some(b) => b

  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match
      case None => None
      case Some(a) => f(a)

  def orElse[B>:A](default: Option[B]): Option[B] = this match
    case None => default
    case Some(_) => this

    // None = None
    // Some(3) = f(3) false => None 
    // Some(5) = f(5) true => Some(5)
    // f(x) = x > 4
  def filter(f: A => Boolean): Option[A] = this match
    case None    => None
    case Some(a) => if f(a) then Some(a) else None

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) match
    case None => None
<<<<<<< HEAD
    case Some(m) => mean(xs.map(x => math.pow(x-m,2)))
    
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a.flatMap(aa => b.flatMap(bb => Some(f(aa,bb))))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case h :: t => h.flatMap(a => sequence(t).map(ts => a::ts))
 
      // h match 
      // case None => None
      // case Some(a) => sequence(t) match
      //   case None => None
      //   case Some(ts) => Some(a::ts)

    //List(SOme(1), Some(2), Some(3)) => Some(List(1,2,3))
    //List(SOme(1), None, Some(3))    => None

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = 
    as.foldRight(Some(Nil): Option[List[B]]) { (a, acc) => 
      // for 
      //   bs <- acc
      //   b  <- f(a)
      // yield b :: bs
      acc.flatMap(bs => f(a).flatMap(b => Some(b :: bs)))
    }
    
//    f(a) => Some(a*2)
// List(Some(1), Some(2), Some(3)) 
//     (Some(1), Some(Nil)) => bs = Nil,  b = 2 acc=> Some(2::Nil)
//     (Some(2), Some(List(2)) =>
//     (Some(3), Some(List(4,2)) =>
//     Some(List(6,4,2))

  
    // case h :: t => 
    //   f(h) match
    //     case None => None
    //     case Some(s) => traverse(t)(f) match
    //       case None => None
    //       case Some(ts) => Some(s::ts)
    //case Some(m) => mean(xs.map(x => math.pow(x - m, 2)))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match
    case None => None
    case Some(aa) => b match 
      case None => None
      case Some(bb) => Some(f(aa,bb))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case h :: t => h match 
      case None => None
      case Some(hh) => sequence(t) match
        case None => None
        case Some(ts) => Some(hh::ts)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil => Some(Nil)
    case h :: t => f(h) match 
      case None => None
      case Some(hh) => traverse(t)(f) match
        case None => None
        case Some(ts) => Some(hh::ts)
