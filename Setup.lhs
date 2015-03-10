#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd
> import System.Exit

> main = defaultMainWithHooks (simpleUserHooks { runTests = suite })
>     where
>     suite _ _ _ _ = system "bash tests.sh" >> return ()

