package localeffect.test

import localeffect.*

// def swap(i: Int, j: Int): ST[S, Unit] =
//   for
//     x <-

def particion[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
  ???
def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = ???

def quicksort(xs: List[Int]): List[Int] =
  if xs.isEmpty then xs
  else
    ST.run(
      [s] =>
        () =>
          for
            arr <- STArray.fromList(xs)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          yield sorted
    )

@main
def test =
  // var result = [s] =>
  //   () =>
  //     for
  //       r1 <- STRef[s, Int](1)
  //       r2 <- STRef[s, Int](1)
  //       x <- r1.read
  //       y <- r2.read
  //       _ <- r1.write(y + 1)
  //       _ <- r2.write(x + 1)
  //       a <- r1.read
  //       b <- r2.read
  //     yield (r1, r2)
  // This one just like the following, that you can construct the result.
  // But you cannot run the result with ST.run. Since it break the type constraint
  // of ST.run that requires the type to be [s] => () => ST[s, A]. which the A here shouldn't contain the s

  // val r = ST.run(result)
  // println(r)

  // val p =
  //   ST.run[STRef[?, Int]]([s] => () => STRef(1).map(ref => ref: STRef[?, Int]))
  // p.write(100)
  // we can't cast p2 to [r] => () => ST[r, Int] since the type of p is ST[p.S, Int]
  // val p2: [r] => () => ST[?, Int] = [r] => () => p.read
  // val p3: [r] => () => ST[?, Int] = p2
  // val p2: [r] => () => ST[r, Int] = [r] =>
  //   () =>
  //     p.write(100)
  //     p.read
  // ST.run[Int](p2)
  // println(ST.run[STRef[?, Int]](p2))

  // this one is also forbidded due to the s and r aren't the same type. even the p2 also choose the [s]
  var result = [s] =>
    () =>
      for
        r1 <- STRef[s, Int](1)
        r2 <- STRef[s, Int](1)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      yield (r1, r2)
