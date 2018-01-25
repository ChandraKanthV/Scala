
object polynomials{

  class Poly(val terms: Map[Int, Int]){
    def this(seq: (Int,Int)*) = this(seq.toMap)

    override def toString: String ={
      terms.toList.sorted.reverse map {
        case (0, c) => c
        case (1, c) => (if(c>1) c else "") + "x"
        case (e, c) => c + "x^" + e
      } mkString "+"
    }

    def + (other:Poly): Poly = new Poly(
      terms ++ (other.terms map (x =>{
        val (e1,c1) = x
        /*
        terms get e1 match {
        case Some(c2) => e1 -> (c2+c1)
        case None => e1 -> c1
      }*/
        e1 -> (c1+ terms.withDefaultValue(0)(e1))
      })))

    def addWithFoldLeft (other: Poly): Poly = new Poly(
      (other.terms foldLeft terms)
        ((t,x) => (t+(x._1 -> (x._2 + t.withDefaultValue(0)(x._1)))) ))
  }


  val p1 = new Poly(1->2, 3->4,0->8, 5->6) //6x^5 + 4x^3 + 2x + 8
  val p2 = new Poly(0->3,3->7)  //7x^3 + 3
  val p3 = new Poly(1->1, 0->1)  //x+1
  val p4 = new Poly(2->2, 0->2)  //2^2x+2


  p3+p4
  p1+p2   //6x^5 + 11x^3 + 2x + 3
  p1 addWithFoldLeft p2
}