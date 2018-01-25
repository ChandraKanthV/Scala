import forcomp.Anagrams.Sentence

import scala.collection.SortedMap
//package forcomp


object Anagrams2 {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  //val dictionary: List[Word] = loadDictionary
  val dictionary: List[Word] = List("eat", "like", "tea", "ate", "tae")

  def wordOccurrences(w: Word): Occurrences = ((w.toLowerCase() groupBy identity) toList) map (pair => {
    val (c, l) = pair;
    (c, l.length)
  }) sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences getOrElse(wordOccurrences(word), List()) //filterNot(word => word.equals("eat"))

  def fun(pair: (Char, Int), zero: List[Occurrences]): List[Occurrences] = pair match {
    case (char, occ) => zero ++ (for {zeroPair <- zero; i <- 1 to occ} yield (char, i) :: zeroPair)
  }
  def combinations(occurrences: Occurrences): List[Occurrences] = (occurrences foldRight List[Occurrences](Nil)) ((a, z) => (fun(a, z)))

  def subtractHelper(x: SortedMap[Char, Int], pair: (Char, Int)):SortedMap[Char, Int] = {
    val (ch,y1) = pair
    val y2 = x(ch) - y1
    println("x is --->", x)
    if (y2 != 0) x updated(ch, y2) else x - ch
  }
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    (y foldLeft SortedMap[Char, Int]() ++ x) ((x, y1) => subtractHelper(x, y1)) toList

  /*
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def iter(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else for {
        combination <- combinations( occurrences )
        word <- dictionaryByOccurrences getOrElse (combination, Nil)
        sentence <- iter( subtract(occurrences,wordOccurrences(word)) )
        if combination.nonEmpty
      } yield word :: sentence
    }
    iter( sentenceOccurrences(sentence) )
  }
  */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentenceAnagramsHelper(sentenceOccurrences(sentence), Nil)
  def sentenceAnagramsHelper(senOcc: Occurrences, senAcc: Sentence): List[Sentence] = senOcc match {
    case Nil => List(senAcc)
    case occur => {
      for {
        com <- combinations(occur)
        word <- dictionaryByOccurrences(com)
        sen <- sentenceAnagramsHelper(subtract(occur, com), senAcc ::: List(word))
      } yield sen
    }
  }


  val sentence = List("Linux", "rulez")
  val anas = List(
    List("Rex", "Lin", "Zulu"),
    List("nil", "Zulu", "Rex"),
    List("Rex", "nil", "Zulu"),
    List("Zulu", "Rex", "Lin"),
    List("null", "Uzi", "Rex"),
    List("Rex", "Zulu", "Lin"),
    List("Uzi", "null", "Rex"),
    List("Rex", "null", "Uzi"),
    List("null", "Rex", "Uzi"),
    List("Lin", "Rex", "Zulu"),
    List("nil", "Rex", "Zulu"),
    List("Rex", "Uzi", "null"),
    List("Rex", "Zulu", "nil"),
    List("Zulu", "Rex", "nil"),
    List("Zulu", "Lin", "Rex"),
    List("Lin", "Zulu", "Rex"),
    List("Uzi", "Rex", "null"),
    List("Zulu", "nil", "Rex"),
    List("rulez", "Linux"),
    List("Linux", "rulez")
  )
  val z =sentenceAnagrams(sentence)
  println("=======>", z)

  //assert(sentenceAnagrams(sentence).toSet === anas.toSet)

  /*
  sentenceOccurrences(List("abcd", "e"))
  val samp =  List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1))


  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
  val y = List(('d',1),('r', 1))


  val z = subtract(x,y)

  val myword = "soon moon! \n aaabc"
  val res = wordOccurrences(myword)

  wordAnagrams("eat")
  val combiOccurance = wordOccurrences("aabb")

  val anagramss = sentenceAnagrams( List("Yes", "man"))

  val z1 = combinations(combiOccurance)
*/

}

