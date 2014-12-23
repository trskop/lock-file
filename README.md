Lock File
=========

[![Hackage](http://img.shields.io/hackage/v/lock-file.svg)][Hackage: lock-file]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


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
    -- From base package, but GHC specific.

import qualified Control.Monad.TaggedException as Exception (handle)
    -- From tagged-exception-core package.
    -- https://github.com/trskop/tagged-exception
import Data.Default.Class (Default(def))
    -- From data-default-class package, alternatively it's possible to use
    -- data-default package version 0.5.2 and above.
    -- http://hackage.haskell.org/package/data-default-class
    -- http://hackage.haskell.org/package/data-default
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
Locking failed with: Unable to acquire lock file: "/var/run/lock/my-example-lock"
$ [1]+  Done                    ./example
```


Building options
----------------

* `-fpedantic` (disabled by default)

  Pass additional warning flags including `-Werror` to GHC during compilation.



[Hackage: lock-file]:
  https://hackage.haskell.org/package/lock-file
  "Hackage: lock-file"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
