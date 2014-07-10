import System.IO
import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

source =
-- show Better with bracketP
    bracketP
        (openFile "input.txt" ReadMode)
        (\handle -> putStrLn "Closing handle" >> hClose handle)
        loop
-- /show
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle

-- show An exception-throwing sink.
exceptionalSink = do
    c <- await
    liftIO $ print c
    error "This throws an exception"
-- /show

-- show We also need to call runResourceT
main = runResourceT $ source $$ exceptionalSink
-- /show