------------------------------------------------------------------------------
-- Game of life implementation
getElem (x,y) cells = (cells !! x) !! y

inBounds cells (x,y) = x >= 0 && x < length cells && y >= 0 && y < length (head cells)

getNeighbors (x,y) cells = filter (inBounds cells) fullList 
    where fullList = [ (x-1, y-1), (x, y-1), (x+1, y-1)
                     , (x-1, y)  ,           (x+1, y)
                     , (x-1, y+1), (x, y+1), (x+1, y+1)
                     ]

evolveCell cells cell = shouldLive
    where neighbors   = getNeighbors cell cells
          aliveNeighs = sum $ map (\x -> if getElem x cells then 1 else 0) neighbors
          shouldLive  | getElem cell cells = aliveNeighs `elem` [2,3]
                      | otherwise          = aliveNeighs == 3

coordCells cells = [[(i,j) | j <- [0..width-1]] | i <- [0..height-1]]
    where height = length cells
          width  = length (head cells)

evolve cells = map (map (evolveCell cells)) $ coordCells cells

------------------------------------------------------------------------------
-- Functions for reading cells from files and printing them to the screen
readCells fname = do
    contents <- readFile fname
    let cells = map (map (=='X')) $ lines contents
    return cells

printCells (cline:cells) = do
    putStrLn $ map (\x -> if x then 'X' else ' ') cline
    if null cells
        then do putStrLn ""
                return ()
        else printCells cells

doGameOfLife (fileName:fileNames) = do
    cells <- readCells fileName
    putStrLn $ fileName ++ ":"
    printCells $ evolve cells
    if null fileNames
        then return ()
        else doGameOfLife fileNames

------------------------------------------------------------------------------
-- Entry point
main = do
    doGameOfLife [ [a,b] | a <- "012", b <- "012" ]
