module Util

import Hedgehog

public export
UnitTest : Type
UnitTest = Property

export
unitTest : PropertyT () -> Property
unitTest = withTests 1 . MkProperty defaultConfig

export
testAll : HasIO io => List (List Group) -> io ()
testAll tss =
  let allTests = foldl (++) (the (List Group) []) tss
    in test allTests

