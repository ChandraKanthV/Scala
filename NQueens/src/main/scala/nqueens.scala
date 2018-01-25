object nqueens {

  private def isSafe(col: Int, queens: List[Int]): Boolean = {
    val currentRow = queens.length
    val queensPos = (currentRow - 1 to 0 by -1) zip queens
    queensPos forall { case (r, c) => (col != c) && (math.abs(col - c) != math.abs(currentRow - r)) }
  }

  private def showBoard(queens:List[Int]) ={
    val rows = for(col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X").mkString
    "\n---------------------\n" + ( rows mkString "\n") + "\n---------------------\n"
  }


  private def placeQueens(k:Int, n:Int) :Set[List[Int]] = {
    if (k==0) Set(List())
    else
      for {
        queens <- placeQueens(k-1, n)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col::queens
  }

  def queens(n: Int) = placeQueens(n,n)
  def board(n:Int) = placeQueens(n,n) map nqueens.showBoard
}
object NQueensMain {
  def main(args: Array[String]) {
    nqueens.queens(4)
    nqueens.board(4)
  }
}