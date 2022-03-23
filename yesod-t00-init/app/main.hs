--------------------------------------------------------------------------------
import           Application (appMain)
import           Prelude     (IO)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

--------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  appMain
