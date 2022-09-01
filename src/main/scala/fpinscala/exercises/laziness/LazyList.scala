package fpinscala.exercises.laziness
import fpinscala.exercises.laziness.LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =     
    @annotation.tailrec
    def inner(lli: LazyList[A], acc: List[A]): List[A] = lli match
        case Cons(h, t) => inner(t(), h() :: acc)
        case Empty => acc.reverse
    inner(this, Nil)

    // LazyList(1,2,3), Nil
    // LazyList(2,3), List(1)
    // LazyList(3), List(2,1)
    // Empty, List(3,2,1)
    // List(1,2,3)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  
  def drop(n: Int): LazyList[A] = this match
    case Cons(_, as) if n > 0 => as().drop(n - 1)
    case _ => this

    //LazyList(1,2,3).drop(-1) => LazyList(1,2,3)
    //(LazyList.Empty).drop(5) => Empty
    //LazyList(1,2,3).drop(2) => LazyList(3)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  
    // LazyList(1,2,3).takeWhile( (x) => x < 2) => LazyList(1)
    // cons(1, LazyList(2,3).takewhile(p))
    // cons(1, empty)

  def forAll(p: A => Boolean): Boolean = 
    //foldRight(true)((x, acc) => acc && p(x))
    this match
      case Empty     => true
      case Cons(h,t) => p(h()) && t().forAll(p)
  
    //LazyList(1,2,3).forAll(odd)    // Cons(1,Cons(2,Cons(3,Empty)))
    //Cons(1, LazyList(2,3)) odd(1) && LazyList(2,3).forall(odd)
    //Cons(2, LazyList(3)) => odd(2) => false

  def headOption: Option[A] = 
    //foldRight(None: Option[A])((a, _) => Some(a))
    this match
      case Cons(a, _) => Some(a())
      case Empty      => None

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = 
    foldRight(Empty: LazyList[B])((x, acc) => cons(f(x), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((x, acc) => if p(x) then cons(x,acc) else acc )
  
  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))
  
  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from( n + 1 ))

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
