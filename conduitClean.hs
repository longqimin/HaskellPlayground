import Data.Conduit
import qualified Data.Conduit.List as CL

source =
    loop 1
  where
    loop i = do
        yieldOr i $ putStrLn $ "Terminated when yielding: " ++ show i
        loop $ i + 1
        
main = source $$ CL.isolate 7 =$ CL.mapM_ print