import Conwayy
import Rndcwboard
import Control.Applicative
import System.Posix.Unistd
import System.Cmd

main = (system "clear") >> procc (cbrd 24) where
  procc b = do 
          b' <- b
          prntConwayb b'
          sleep 3
          system "clear"
          procc $ return (nxtGen b')
