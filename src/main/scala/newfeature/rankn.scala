package newfeature

val id = [A] => (a: A) => a
def rank2[B, C](a: (B, C), doSomething: [A] => A => A): (B, C) = (doSomething(a._1), doSomething(a._2))

@main
def test = 
  println(
    rank2((1, ""), id)
  )