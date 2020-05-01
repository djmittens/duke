package example

/**
  * Peano number implementation for type leven arythmetic using 
  * taken from https://wiki.haskell.org/Peano_numbers#Peano_number_types
  * More information can be found here
  * https://github.com/Joe-Edwards/type-arithmetic/blob/master/src/main/scala/com/softwire/NaturalNumbers.scala
  * https://www.softwire.com/insights/arithmetic-using-the-scala-type-system/
  */
object Peano {

    sealed class Nat  {
        type Add[A <: Nat] <: Nat
        type Sub[A <: Nat] <: Nat
        type Mul[A <: Nat] <: Nat
        type Div[A <: Nat] <: Nat

        type IfZero[T <: Nat, F <: Nat] <: Nat
        type IfLessThan[A <: Nat, T <: Nat, F <: Nat] <: Nat

        type Succ <: Nat
        type Pred <: Nat

    }

    case object Zero extends Nat {
        type Add[A <: Nat] = A
        type Sub[A <: Nat] = A
        type Mul[A <: Nat] = Zero.type
        type Div[A <: Nat] = Zero.type

        type IfZero[T <: Nat, F <: Nat] = T
        type IfLessThan[A <: Nat, T <: Nat, F <: Nat] = A#IfZero[F, T]

        type Succ = Peano.Succ[_0]
        type Pred = Nothing
    }

    class Succ[B <: Nat] extends Nat { self =>
        type Add[A <: Nat] = Peano.Succ[B#Add[A]]
        type Sub[A <: Nat] = B#Sub[A]
        type Mul[A <: Nat] = A#Add[A#Mul[B]]
        type Div[A <: Nat] = B#Succ#IfLessThan[A, _0, A#Sub[B#Succ]#Div[A]#Succ]

        type IfZero[T <: Nat, F <: Nat] = F
        type IfLessThan[A <: Nat, T <: Nat, F <: Nat]  = A#IfZero[F, B#IfLessThan[A#Pred, T, F]]

        type Succ = Peano.Succ[self.type]
        type Pred = B
    }


    final case class  ToInt[A <: Nat] (value: Int)
    object ToInt {
        def apply[A <: Nat : ToInt]: Int =  implicitly[ToInt[A]].value
        implicit val zero: ToInt[_0] = ToInt(0)
        implicit def succ[A <: Nat : ToInt]: ToInt[Succ[A]] = ToInt(ToInt[A] + 1)
    }


    type +[A <: Nat, B <: Nat] = A#Add[B]
    type -[A <: Nat, B <: Nat] = A#Sub[B]
    type *[A <: Nat, B <: Nat] = A#Mul[B]
    type /[A <: Nat, B <: Nat] = A#Div[B]

    type _0 =  Zero.type
    type _1 =  Succ[_0]
    type _2 =  Succ[_1]
    type _3 =  Succ[_2]
    type _4 =  Succ[_3]
    type _5 =  Succ[_4]
    type _6 =  Succ[_5]
    type _7 =  Succ[_6]

    def main(args: Array[String]): Unit = {
        println(implicitly[_2 * _3 =:= _6])
        println(ToInt[_7 * _7 ])
    }

} 
