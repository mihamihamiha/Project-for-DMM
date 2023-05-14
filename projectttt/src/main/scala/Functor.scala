import scala.util.{Success,Try}
object Functor extends App {

  val List1: List[Int] = List(1, 2, 3, 4)
  val Option1: Option[Int] = Some(5)
  val Try1: Try[Int] = Success(20)
  //Calculating the double of them by using map function as it is:
  val CalcList = List1.map(x => x * 2)
  val CalcOption = Option1.map(x => x * 2)
  val CalcTry = Try1.map(x => x * 2)
  println("Using non-functor:")
  println(CalcList)
  println(CalcOption)
  println(CalcTry)

  //Factors for those:
  def DoubleList(List1: List[Int]): List[Int] = List1.map(_ * 2)
  def DoubleOption(Option: Option[Int]): Option[Int] = Option.map(_ * 2)
  def DoubleTry(Try: Try[Int]): Try[Int] = Try.map(_ * 2)
  println("Using 'duplicated',simple factors:")
  println(DoubleList(List1))
  println(DoubleOption(Option1))
  println(DoubleTry(Try1))
  //Notice that the print results are the exact same

  trait Functor[C[_]] {
    def map[A, B](container: C[A])(f: A => B): C[B]
  }

  implicit val ListFunctor = new Functor[List] {
    def map[A,B](listValue: List[A])(f: A => B): List[B] = listValue.map(f)
  }
  implicit val OptionFunctor = new Functor[Option] {
    def map[A,B](optionValue: Option[A])(f: A => B): Option[B] = optionValue.map(f)
  }
  implicit val TryFunctor = new Functor[Try] {
    def map[A, B](tryValue: Try[A])(f: A => B): Try[B] = tryValue.map(f)
  }
  val double=(x:Int)=>x*2
  println("Using a stable, general factor:")
  println(ListFunctor.map(List1)(double))
  println(OptionFunctor.map(Option1)(double))
  println(TryFunctor.map(Try1)(double))
}