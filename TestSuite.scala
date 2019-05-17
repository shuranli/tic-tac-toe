import Solution._
import hw.tictactoe._
class TestSuite extends org.scalatest.FunSuite{
	//Game (turn : Player, dim: Int, board: Map[(Int,Int),Player])
	val Gameboard:Game =Solution.createGame(X,3,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))
	val NotFinish:Game =Solution.createGame(X,3,Map((0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))
	val tie:Game=Solution.createGame(X,3,Map((0,0)->O,(0,1)->O,(0,2)->X,(1,0)->X, (1,1)->O,(1,2)->O,(2,0)->O,(2,1)->X,(2,2)->X))
	val Fullboard:Game =Solution.createGame(X,3,Map((1,0)->X,(0,1)->X, (0,2)->X, (1,2)-> X,(2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))
	val diaWin:Game= Solution.createGame(X,3,Map((2,0)->O,(1,1)->O,(0,2)->O,(0,0)->X))
test ("test getRows"){
	assert(Gameboard.getRows(0,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(O,X))
	assert(Gameboard.getRows(1,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(O,X))
	assert(Gameboard.getRows(2,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(X))
}
test ("test getCols"){
	assert(Gameboard.getCols(0,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(O))
	assert(Gameboard.getCols(1,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(O))
	assert(Gameboard.getCols(2,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(X,X,X))
	assert(Gameboard.getCols(2,Map((0,0)-> O)) == List())
}
test ("test Dia"){
	assert(Gameboard.getDiagonal(Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(O,O,X))
	assert(Gameboard.getAntiDiagonal(3,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X)) == List(X,O))
}
test("LinehasWinner"){
	assert(Gameboard.LinehasWinner(List(X,O),3)==false)
	assert(Gameboard.LinehasWinner(List(X,O,X),3)==false)
	assert(Gameboard.LinehasWinner(List(X,X,X),3)==true)
}
test("colhaswinner"){
	assert(Gameboard.ColshasWinner(2,3,Map((0,0)->O,(2,1)->X,(1,0)->O,(2,2)->X,(2,0)->O))==false)
	assert(Gameboard.ColshasWinner(2,3,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))==true)
}
test("diahasWinner"){
	assert(Gameboard.DiahasWinner(3,Map((0,0)->O,(1,0)->X,(1,1)-> O, (2,1)-> X, (2,2)->O))==true)
	assert(Gameboard.DiahasWinner(3,Map((0,0)->O,(2,1)->X,(1,0)->O,(2,2)->X,(2,0)->O))==false)
}
test("AntiDiahasWinner"){
	assert(Gameboard.AntiDiahasWinner(4,Map((3,0)->X,(2,1)->X,(1,2)->X,(0,3)->X))==true)
	assert(Gameboard.AntiDiahasWinner(3,Map((0,0)->O,(2,1)->X,(1,0)->O,(2,2)->X,(2,0)->O))==false)
}
test("rowshaswinner"){
	assert(Gameboard.RowshasWinner(2,3,Map((0,0)->O,(2,1)->X,(1,0)->O,(2,2)->X,(2,0)->O))==true)
	assert(Gameboard.RowshasWinner(2,3,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))==false)
}
test("checkisFinished"){
	assert(Gameboard.isFinished()==true)
	assert(Fullboard.isFinished()==true)
	assert(NotFinish.isFinished()==false)
}
test("getWinner"){
	assert(Gameboard.getWinner()==Some(X))
	assert(tie.getWinner()==None)
	assert(diaWin.getWinner()==Some(O))
}
test("getWinnerCols"){
	assert(Gameboard.getWinnerCols(2,3,Map((2,0)-> X, (0,0)-> O, (2,1)-> X, (1,1)-> O,(2,2)->X))==Some(X))
}
test("fillOne"){
	val Gameboard1:Game =Solution.createGame(X,3,Map((0,0)->X,(1,0)->X, (2,0)->O,(1,1)->O,(2,1)->O,(0,2)->X, (2,2)-> X))
	assert(Gameboard1.FillOneCols(2,2,Map((0,0)->X,(1,0)->X, (2,0)->O,(1,1)->O,(2,1)->O,(0,2)->X, (2,2)-> X),O)==List())
}
test("getNextBoard"){
	val Gameboard1:Game =Solution.createGame(O,3,Map((0,0)->X,(1,0)->X, (2,0)->O,(1,1)->O,(2,1)->O,(0,2)->X, (2,2)-> X))
	assert(Gameboard1.nextBoards()==List(createGame(X,3,Map((0,0)->X,(1,0)->X, (2,0)->O,(1,1)->O,(2,1)->O,(0,2)->X, (2,2)-> X, (1,2)-> O)),createGame(X,3,Map((0,0)->X,(1,0)->X, (2,0)->O,(1,1)->O,(2,1)->O,(0,2)->X, (2,2)-> X,(0,1)->O))))
}
}