package com.felixmilea.algorithms

import com.felixmilea.profiling.FunctionTimer

object AttackingQueens extends App
{
	val ft = new FunctionTimer

	for (n <- (1 to 50))
	{
		val board = Array.ofDim[Boolean](n,n)
		val solved : Boolean = ft.time { AttackingQueensAlgorithm(n, board) }.asInstanceOf[Boolean]
		
		printf("%d. %s\n", n, if (solved) "Solved" else "Failed")
		
		if (solved)
		{
			printBoard(board)
			printf("\nCalculation Time: %dms\n\n", ft.elapsedTime)		  
		}
	}

	def AttackingQueensAlgorithm (n : Int, board : Array[Array[Boolean]], row : Int = 0): Boolean =
	{
		if (row == n)
			return true

		var col : Int = 0
		while (col < n)
		{
			if (!checkTopQueens(col, row, board) && !checkTopLeftQueens(col, row, board) && !checkTopRightQueens(col, row, board))
			{
				board(col)(row) = true
				if (AttackingQueensAlgorithm(n, board, row + 1))
					return true
				else
					board(col)(row) = false
			}
		
			col += 1
		}
		
		return false
	}
	
	def checkTopQueens(col: Int, row: Int, board : Array[Array[Boolean]]) : Boolean =
	{
		var y : Int = row - 1
		while (y >= 0)
		{
			if (board(col)(y)) return true
			y -= 1
		}
		
		return false
	}
	
	def checkTopLeftQueens(col: Int, row: Int, board : Array[Array[Boolean]]) : Boolean =
	{
		var y : Int = row - 1
		var x : Int = col - 1
		while (y >= 0 && x >= 0)
		{
			if (board(x)(y)) return true
			y -= 1
			x -= 1
		}
		return false		
	}
	
	def checkTopRightQueens(col: Int, row: Int, board : Array[Array[Boolean]]) : Boolean =
	{
		var y : Int = row - 1
		var x : Int = col + 1
		while (y >= 0 && x < board.length)
		{
			if (board(x)(y)) return true
			y -= 1
			x += 1
		}
		return false
	}
	
	def printBoard (board : Array[Array[Boolean]]) =
	{
		val nOffset : String = if (board.length > 9) " " else ""
		  
		// print x-axis labels
		printf("   %s%s\n", nOffset,
		    (1 to board.length).foldLeft("")((b,a) => {
		    	b + String.format("%d%s%s", a.asInstanceOf[Integer], nOffset, (if (a > 9) "" else " "))
		    }))

		for (x <- (0 to board.length - 1))
		{
			printf("%s%d ", if (x + 1 > 9) "" else " ", x + 1)
			for (y <- (0 to board.length - 1))
			{
				printf("%s%s ", nOffset, if (board(x)(y)) "Q" else "-")
			}
			println
		}
	}

}