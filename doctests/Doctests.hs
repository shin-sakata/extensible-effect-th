module Doctests where

import Test.DocTest (doctest)

main = doctest ["-isrc", "src"]