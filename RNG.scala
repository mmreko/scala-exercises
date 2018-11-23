package state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    /* => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a,b), rb2)
    }*/
   //ooooor
    /*flatMap(ra) ( a => {


        flatMap(rb)(b => unit(f(a, b)))

      }
    )*/
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, newRng) = f(rng)
      g(a)(newRng)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (number, newState) = rng.nextInt
    if (number < 0) (Math.abs(number + 1), newState)
    else (number, newState)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (num1, newState1) = rng.nextInt
    val (num2, newState2) = double(newState1)
    ((num1, num2),newState2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (num1, state1) = double(rng)
    val (num2, state2) = state1.nextInt
    ((num1, num2), state2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (num1, state1) = double(rng)
    val (num2, state2) = double(state1)
    val (num3, state3) = double(state2)
    ((num1, num2, num3), state3)
  }

  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def double: Rand[Double] = {
    map(nonNegativeInt)(num => num / (Integer.MAX_VALUE + 1))
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => {
      val (b, s2) = run(sb)
      //or sb.map.....
      map( b => f(a,b))
    })

  // A => S => (B, S)
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S,A] =
    new State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    //fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    def loop(s: S, actions: List[State[S, A]], res: List[A]): (List[A], S) = {
      actions match {
        case Nil => (res.reverse, s)
        case h :: t => {
          val (a, newS) = h.run(s)
          loop(newS, t, a :: res)
        }
      }
    }
    State(s => loop(s, fs, List()))
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
