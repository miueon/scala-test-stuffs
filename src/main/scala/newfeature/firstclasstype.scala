package newfeature.firstclasstp
import scala.collection.immutable.Nil
type IsSingleton[X <: Boolean] = X match
  case true  => Int
  case false => List[Int]

// def sum(single: Boolean, x: IsSingleton[single.type]): Int = (single, x) match
//   case (true, x: IsSingleton[true])     => x
//   case (false, ((x: Int) :: (xs: IsSingleton[false]))) =>
    // sum(false, xs) + x

@main
def test = 

  ???
