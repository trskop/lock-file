Lock File
=========


Description
-----------

Provide exclusive access to a resource using lock file, which are files whose
purpose is to signal by their presence that some resource is locked.


Usage Example
-------------

Following example acquires lock file and then waits `1000000` micro seconds
before releasing it. Note also that it is possible to specify retry strategy.
Here we set it to `No` and therefore this code won't retry to acquire lock file
after first failure.

```Haskell
import Control.Concurrent (threadDelay)

import qualified Control.Monad.TaggedException as Exception (handle)
import Data.Default.Class (Default(def))
import System.IO.LockFile
    ( LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , withLockFile
    )


main :: IO ()
main = Exception.handle (putStrLn . ("Locking failed with: " ++) . show)
    . withLockFile def{retryToAcquireLock = No} "/var/run/lock/my-example-lock"
    $ threadDelay 1000000
```

This command line example shows that trying to execute two instances of
`example` at the same time will result in failure of the second one.

```
$ ghc example.hs 
[1 of 1] Compiling Main             ( example.hs, example.o )
Linking example ...
$ ./example & ./example 
[1] 7893
Locking failed: Unable to acquire lock file: "/var/run/lock/my-example-lock"
$ [1]+  Done                    ./example
```


Building options
----------------

* `-fpedantic` (disabled by default) --
  Pass additional warning flags including `-Werror` to GHC during compilation.
