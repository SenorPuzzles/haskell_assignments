import System.Environment
import System.Directory
import System.IO
import Data.List


main = do
    (commad:args) <- getArgs
    let (Just action) = lookup commad dispatch
    action args


dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStr $ unlines numberedTasks


remove :: [String] -> IO ()
remove [filename, numberString] = do
    handleTaksRemove <- openFile filename ReadWriteMode
    contents <- hGetContents handleTaksRemove
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks

    withFile filename WriteMode $ \handleWrite -> do
        hPutStr handleWrite (unlines newTodoItems)

    hClose handleTaksRemove
