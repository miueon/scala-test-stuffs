package lightweight

trait ArraySearch[A]:
  opaque type T = A

  def sort(arr: Array[Int]): T

  def search(a: Int, arr: T): Boolean

  extension (t: T) inline protected def value: A = t

  protected inline def apply(a: A): T = a

object ArrSearch extends ArraySearch[Array[Int]]:

  def search(a: Int, arr: T): Boolean =
    arr.value.contains(a)

  def sort(arr: Array[Int]): T =
    apply(arr.sorted)

trait Index:
  opaque type Index = Int
  opaque type IndexL = Int
  opaque type IndexH = Int

  def lwb: IndexL
  def upb: IndexH
  def bsucc(i: Index): IndexL
  def bpred(i: Index): IndexH
  def bmid(x: Index, y: Index): Index
  def bcmp(i: IndexL, j: IndexH): Option[(Index, Index)]

object IndexF:
  def apply(upbValue: Int): Index = new:
    def lwb: IndexL = ???
    def bcmp(i: IndexL, j: IndexH): Option[(Index, Index)] = ???
    def bmid(x: Index, y: Index): Index = ???
    def bpred(i: Index): IndexH = ???
    def bsucc(i: Index): IndexL = ???
    def upb: IndexH = ???

@main
def test =
  val arr = Array(1, 2, 3, 4, 5)
  val sortedArr = ArrSearch.sort(arr)
  println(ArrSearch.search(3, sortedArr)) // true
  println(ArrSearch.search(6, sortedArr)) // false
