import hw.tictactoe._
class Game (turn : Player, dim: Int, board: Map[(Int,Int),Player]) extends GameLike [ Game ] {
def isFinished (): Boolean = {
	//tie, all space is full 
	if(board.size==dim*dim)
		true
	else if(ColshasWinner(dim-1,dim,board))
		true
	else if(RowshasWinner(dim-1,dim,board))
		true
	else if(DiahasWinner(dim,board))
		true
	else if(AntiDiahasWinner(dim,board))
		true
	else
		false
}
def getBoard():Map[(Int,Int),Player]=this.board

def getTurn():Player=this.turn

def getRows(Num: Int, board: Map[(Int,Int),Player]):List[Player]= {
	board.filter({case((x,y),v)=> (y==(Num))}).values.toList
}
def getCols(Num: Int, board: Map[(Int,Int),Player]):List[Player]= {
	board.filter({case((x,y),v)=> (x==(Num))}).values.toList
}
def getDiagonal(board: Map[(Int,Int),Player]):List[Player]= {
	board.filter({case((x,y),v)=> (x==y)}).values.toList
}	
def getAntiDiagonal(dim: Int, board: Map[(Int,Int),Player]):List[Player]= {
	board.filter({case((x,y),v)=> (x+y)==dim-1}).values.toList
}	
def RowshasWinner(rows:Int, dim:Int, board: Map[(Int,Int),Player]):Boolean= rows match{
	case rows if (rows < 0) =>false 
	case _ => {
		if(LinehasWinner(getRows(rows,board),dim))
			true
		else
			RowshasWinner(rows-1,dim,board)
	}
}


def ColshasWinner(cols:Int, dim:Int, board: Map[(Int,Int),Player]):Boolean = cols match{
		case cols if (cols < 0) =>false 
		case _ => {
		if(LinehasWinner(getCols(cols,board),dim))
			true
		else
			ColshasWinner(cols-1,dim,board)
	}
}
def DiahasWinner(dim:Int, board: Map[(Int,Int),Player]):Boolean = {
	if(LinehasWinner(getDiagonal(board),dim))
			true
		else
			false
}
def AntiDiahasWinner(dim:Int, board: Map[(Int,Int),Player]):Boolean = {
	if(LinehasWinner(getAntiDiagonal(dim,board),dim))
			true
		else
			false
}
def LinehasWinner(line:List[Player], dim: Int):Boolean =  {
	if (line.size!=dim)
		false 
	else line match 
	{
		case Nil=>true
		case x::Nil=> true
		case x::y::Nil =>
		{
			if(x==y)
				true
			else false 
		}
		case x::y::t => {
			if(x==y)
				LinehasWinner(y::t,dim-1)
			else 
				false
		}
	}
}


def getWinnerCols (cols:Int, dim:Int, board: Map[(Int,Int),Player]): Option[Player] = cols match{
	case cols if (cols < 0) => None 
		case _ => {
		if(LinehasWinner(getCols(cols,board),dim))
			Some(getCols(cols,board).head)
		else
			getWinnerCols(cols-1,dim,board)
		}
}

def getWinnerRows(rows:Int, dim:Int, board: Map[(Int,Int),Player]):Option[Player] = rows match{
	case rows if (rows < 0) =>None 
	case _ => {
		if(LinehasWinner(getRows(rows,board),dim))
			Some(getRows(rows,board).head)
		else
			getWinnerRows(rows-1,dim,board)
	}
}

def getWinnerDia(dim:Int, board: Map[(Int,Int),Player]):Option[Player] = {
	if(LinehasWinner(getDiagonal(board),dim))
			Some(getDiagonal(board).head)
		else
			None
}

def getWinnerAntiDia(dim:Int, board: Map[(Int,Int),Player]):Option[Player] = {
	if(LinehasWinner(getAntiDiagonal(dim,board),dim))
			Some(getAntiDiagonal(dim,board).head)
		else
			None
}
/* Assume that isFinished is true */
def getWinner (): Option [ Player ] = {
	if(ColshasWinner(dim-1,dim,board))
		getWinnerCols(dim-1,dim,board)
	else if(RowshasWinner(dim-1,dim,board))
		getWinnerRows(dim-1,dim,board)
	else if(DiahasWinner(dim,board))
		getWinnerDia(dim,board)
	else if(AntiDiahasWinner(dim,board))
		getWinnerAntiDia(dim,board)
	else
		None
}

def nextPlayer(currPlayer:Player):Player = {
	if (currPlayer==X)
		O
	else X
}

def FillOneCols(val1:Int,val2:Int, board: Map [( Int , Int ) , Player ],currPlayer:Player):List[Game] = val2 match {
	case val2 if (val2 < 0) => Nil 
	case _ =>{
		if (board.keys.exists(_ ==(val1,val2)))
			FillOneCols(val1,val2-1,board,currPlayer)
		else
			new Game(nextPlayer(currPlayer),dim,board+((val1,val2)->currPlayer)) :: FillOneCols(val1,val2-1,board,currPlayer)
		}

}

def FillAllCols(val1:Int,val2:Int, board: Map [( Int , Int ) , Player ],currPlayer:Player):List[Game] = val1 match {
	case val1 if (val1 < 0) => Nil 
	case _ => {
		FillOneCols(val1,val2,board,currPlayer)::: FillAllCols(val1-1,val2,board,currPlayer)
	}

}
def nextBoards (): List [ Game ] = turn match {
    case turn if (turn==X) => 
		FillAllCols(dim-1,dim-1,board,X)
	case _ => FillAllCols(dim-1,dim-1,board,O)
}

}

object Solution  extends MinimaxLike{
	val x: Player = X
	val o: Player = O
	val GameBoard:Map[(Int,Int),Player]= Map()
	type T = Game // T is an " abstract type member " of MinimaxLike
	def createGame ( turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]): Game = {
		new Game(turn, dim, board)
	}

	def minimax(board: Game): Option[Player] = {
		val currPlayer: Player= board.getTurn()
		if(board.isFinished()){
			board.getWinner()
		}
		else{
			val FinalWinner = board.nextBoards().map(x => minimax(x))
			if(FinalWinner.contains(None)){
				None
			}
			else if(FinalWinner.contains(Some(currPlayer))){
				Some(currPlayer)
			}
			else
			{
				if(currPlayer==X)
					Some(O)
				else 
					Some(X)
				
			}
			
		}
	}

	
}