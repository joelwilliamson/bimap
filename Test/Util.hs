{-# LANGUAGE TemplateHaskell #-}
module Test.Util (
    extractTests,
) where

import Control.Arrow
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import Text.Printf


{-
Use 'propertyNames' to extract all QuickCheck test names from
a file.
-}
fileProperties :: FilePath -> IO [String]
fileProperties = fmap propertyNames . readFile

{-
Find all the tokens in a file that
  1) are the first token on a line, and
  2) begin with "prop_".
-}
propertyNames :: String -> [String]
propertyNames = 
    lines >>> map firstToken >>> filter isProperty >>> nub
    where
    firstToken = fst . head . lex
    isProperty = isPrefixOf "prop_"

{- Inspired by & borrowed from: -}
-- http://blog.codersbase.com/2006/09/01/simple-unit-testing-in-haskell/
mkCheck name =
    [| printf "%-25s : " name >> quickCheck $(varE (mkName name)) |]

mkChecks []        = undefined -- if we don't have any tests, then the test suite is undefined right?
mkChecks [name]    = mkCheck name
mkChecks (name:ns) = [| $(mkCheck name) >> $(mkChecks ns) |]

{-
Extract the names of QuickCheck tests from a file, and splice in
a sequence of calls to them. The module doing the splicing must
also import the file being processed.
-}
extractTests :: FilePath -> Q Exp
extractTests = (mkChecks =<<) . runIO . fileProperties
