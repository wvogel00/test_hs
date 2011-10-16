mport Control.Monad

width  = 5
height = 5

type Pos = (Int,Int)
type Living = [Pos]

positions :: [Pos]
positions = [(x,y) | x <- [1..width] , y <- [1..height]]

start :: Living
start = [(2,1),(3,1),(1,2),(3,2),(5,2),(4,3),(1,4),(2,5),(3,5)]

surround :: Pos -> [Pos]
surround (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
                (x-1,y),            (x+1,y),
                (x-1,y+1),(x,y+1),(x+1,y+1)]

alive :: Pos -> Living -> Int
alive pos lifeState = length.filter (`elem` lifeState) $ surround pos

visual :: Living -> IO()
visual lives = let lifelist = visualFormat$map (f.(`elem` lives)) positions
               in forM_ lifelist print >> putStrLn ""
  where
    f a  = if a then '*' else '-'

visualFormat :: String -> [String]
visualFormat [] = []
visualFormat ls = a:visualFormat b
  where
    (a,b) = splitAt width ls

next :: Living -> Pos -> Pos
next lives pos = let n = alive pos lives
                 in case elem pos lives of
                     True | elem n [2,3] -> pos
                     False| n == 3 -> pos
                     otherwise -> (0,0)

format :: Living -> Living
format = filter (not.(==(0,0)))

showNext :: Int -> Living ->IO()
showNext 0 _ = print "stop"
showNext n lives = do visual lives
                      s <- wait 10000
                      showNext (n-1) $ format.map (next lives) $ positions
                    
wait :: Int -> IO Int
wait a = return $ foldl (+) 0 [1..a]

main = showNext 10 start

