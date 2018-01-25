import math.Ordering
import scala.runtime.ScalaRunTime

def mergsort[T]( inputList:List[T])(implicit ord:Ordering[T]):List[T] ={
  val halfSize = inputList.length/2
  if(halfSize == 0) inputList
  else {
    def merge(fh:List[T], sh:List[T] ):List[T] = {
      //println("entred withfh=["+fh+"] and sh =["+sh+"]")
      (fh,sh) match {
        case (Nil, ys) => ys
        case(xs, Nil) => xs
        case(x::xs, y::ys) => if (ord.lt(x,y)) x::merge(xs,sh)
        else y::merge (fh,ys)
      }
    }
    val (firstHalf,secondHalf) = inputList splitAt halfSize
    //println("calling merge with fh=["+firstHalf +"] and sh=["+ secondHalf +"]")
    merge(mergsort(firstHalf),mergsort(secondHalf))
  }
}

val numList = List(9,8,7,6,5,4,3,2,1,0)
val stringList  = List("zebra", "piano", "apple", "Nose", "nose", "Nose", "Apple", "Apple" )


val sortedNumList = mergsort(numList)
val sortedStringList = mergsort(stringList)


def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y*y) :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

val sq1 = squareList1(numList)
val sq2 = squareList2(numList)

def posElement1(xs: List[Int]) :List[Int] = xs match {
  case Nil => xs
  case y::ys => if(y>0) y::posElement1(ys) else posElement1(ys)
}
def posElement2(xs: List[Int]) = xs filter (x => (x>0))

val posList = List(-4, -3, -2, -1, 1, 2, 3, 4)
val posList1 = posElement1(posList)
val posList2 = posElement2(posList)
val (part1, part2) = posList partition (x => x>0)
val parts = posList partition (x => x<0)
    posList take (posList.length/2)
    posList drop (posList.length/2)

    posList takeRight(posList.length/2)
    posList dropRight(posList.length/2)

    posList takeWhile( x => x>0)
    posList takeWhile (x => x<0)
    posList dropWhile(x => x<0)
    posList span (x => x<0 )

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (fir, rest) = xs span( y => y==x)
    fir :: pack(rest)
  }
}
pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs:List[T]):List[(T, Int)] = pack(xs) map (x=> (x.head, x.length))
encode(List("a", "a", "a", "b", "c", "c", "a"))

def sumAllL(xs:List[Int]) = (0::xs) reduceLeft(_ + _)
def sumAllR(xs:List[Int]) = (0::xs) reduceRight (_ + _)

val sumnumallL =  sumAllL (numList)
val sumnumallR = sumAllR(numList)

    (numList foldLeft 0)( _ + _ )
    (numList.dropRight(2).takeRight(3) foldRight  1)( (l,r) => l * r )

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U] () )( (a,z) => (f(a):: z ) )


def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (a,z) => z+1 )

  lengthFun(numList)
  val z1 = mapFun(numList, (x:Int) => x*x)

val nList = List(1,2,3)
val combineList = (numList foldRight nList) (_ :: _)
  stringList map (c => List(" and ", c))
  stringList flatMap  (c => List(" and ", c))

def scalarProduct (xs: Vector[Double], ys:Vector[Double]) : Double = (xs zip ys).map {case (x,y) => x*y}.sum
def scalarProduct2(xs: Vector[Double], ys:Vector[Double]): Double = (for ((x,y) <- xs zip ys ) yield (x*y)).sum

scalarProduct(Vector(1.0,2.0,3.0,4.0), Vector(4.0,3.0,2.0,1.0))
scalarProduct2(Vector(1.0,2.0,3.0,4.0), Vector(4.0,3.0,2.0,1.0))



def isPrime(n: Int) : Boolean = (2 until n) forall(p => (n%p !=0) )
isPrime(4)

// all pairs of i, j where 1<j<i<n such that i+j is a prime
def ijPrime(n:Int) = {
  (1 until n) flatMap ( i =>
    (1 until i) map (j => (j,i))
  ) filter (  item => isPrime(item._1 + item._2 ))
}
val n =7
val seven = (1 until n) flatMap ( i =>
  (1 until i) map (j => (i,j))
  )
seven.head
val seven5 = ijPrime(5)

val seven2 = for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i+j)
} yield (j,i)



