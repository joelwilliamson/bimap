#!/bin/bash

rm .test.log 2>/dev/null
runhaskell Test/RunTests.hs > .test.log || exit 1
cat .test.log || exit 2
grep Falsifiable .test.log >/dev/null && exit 3
echo "~~ all tests passed ~~"
exit 0
