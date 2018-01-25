import scala.math.Ordering
import scala.util.parsing.combinator.Parsers

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

def weight(tree: CodeTree): Int = tree match {
  case Leaf(_,w) => w
  case Fork(_ ,_ ,_ ,w) => w
}

def chars(tree: CodeTree): List[Char] = tree match {
  case Leaf(char, _) => List(char)
  case Fork(_ ,_ , chars,_) => chars
}

def times(chars: List[Char]): List[(Char, Int)] = chars match {
  case Nil => Nil
  case x::xs => (x,chars.count(_ == x)):: times(xs filterNot(_ == x))
}

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
  def iter(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case Nil => Nil
    case x :: xs => Leaf(x._1, x._2) :: makeOrderedLeafList(xs)
  }
  iter(freqs) sortWith (_.weight < _.weight)
}

val orderedList = makeOrderedLeafList(times(List('a','b','a','c','b')))

def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

def combine(trees: List[CodeTree]): List[CodeTree] = {
  def iter(trees: List[CodeTree]) = trees match {
    case Nil => trees
    case _ :: Nil => trees
    case ct1::ct2::cts => Fork(ct1,ct2,chars(ct1):::chars(ct2), weight(ct1)+weight(ct2) ) :: cts
  }
  iter(trees).sortWith((ct1,ct2) => weight(ct1) < weight(ct2))
}
val combinedList = combine(orderedList)


def until(fSingleton: List[CodeTree] => Boolean, fCombine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
  if(fSingleton (trees)) trees
  else
    until(fSingleton, fCombine) (combine(trees))
}
until(singleton, combine)(orderedList)

def createCodeTree(chars: List[Char]): CodeTree =
  until(singleton, combine)(makeOrderedLeafList(times(chars))).head


val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

type Bit = Int
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  /*
  def iter(iterTree: CodeTree, iterBits: List[Bit]): List[Char] = {
    (iterTree, iterBits) match {
      case (Leaf(char, _), Nil) => char :: Nil
      case (Leaf(char, _), b :: bs) => char :: iter(tree, iterBits)
      case (Fork(_, _, _, _), Nil) => throw new Error("Imcomplete sequence")
      case (Fork(left, right, _, _), b :: bs) => iter(if (iterBits.head == 0) left else right, bs) //bit is consumed here atr leaf level
    }
  }
  iter(tree,bits)
  */

  //NOTE
  //abovce code is factorial type tail recurssion so it runs slower
  // Below code is GCD type tail recurssion, Hence it runs faster
  def readChar(subTree: CodeTree, iterBits: List[Bit]):(Char, List[Bit]) = {
    subTree match {
      case Leaf(char, _) => (char,iterBits)
      case Fork(left, right,_,_) => if (iterBits != Nil)
        readChar(if(iterBits.head == 0) left else right, iterBits.tail)
        else
        throw new Error("Incomplete bit sequence")
    }
  }
  def readIter(iterBits: List[Bit]): List[Char] ={
    iterBits match{
      case Nil => Nil
      case _ =>
        val (c,bits) = readChar(tree,iterBits)
        c::readIter(bits)
    }
  }
  readIter(bits)
}

val myCodeTree = createCodeTree(List('a','b','a','c','b'))
val d = decode(myCodeTree,List(0,0,1,0,1,1,1,1,1,0))


def traversTree(tree: CodeTree, char: Char): List[Bit]= {
  def iterTraverse(iterTree: CodeTree) : (List[Bit],Boolean) = {
    iterTree match {
      case Leaf(c, _) => if (c == char) (Nil, true) else (Nil, false)
      case Fork(lt, rt, _, _) =>
        val (ltPath, ltBool) = iterTraverse(lt)
        val temp = List(0)
        if (ltBool) (temp ++ ltPath, ltBool)
        else {
          val (rtPath, rtBool) = iterTraverse(rt)
          if (rtBool) (1 :: rtPath, rtBool)

          else (Nil, false)//throw new Error("Character '" + char + "' not found")
        }
    }
  }
  iterTraverse(tree)._1
}
def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(c=>traversTree(tree,c))

val b = encode(myCodeTree) (List('a'))

def removeAt(n: Int, xs: List[Char]):List[Char] = {
  println(n +"\n")
  xs match {
    case Nil => Nil
    //case x :: Nil => List(1)
    case x :: xs => if(n > 0)(x::removeAt(n-1,xs)) else xs
  }
}
val charlist = List('a', 'b', 'c', 'd')
val r2 = removeAt(4, charlist) // List(a, c, d)

