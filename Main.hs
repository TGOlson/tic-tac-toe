import Game.Game
import Board.Board
import Symbol


main :: IO Board
main = play X $ makeBoard 9
