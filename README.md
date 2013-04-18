Listing: Unifying list-like data structures
===========================================

TODO: make Length (or Size) a separate class.

This library attemps to solve the painful un-unified similarities
between Map, HashMap, Set, List, Maybe, Sequence, Vector, Array and so
forth by providing a single interface that allows one to perform many
common functions on these data structures without having to import a
whole host of qualified modules.

The Listing class is still not finalized but has many useful functions
in it already and is quite usable.

This library
------------

This library only defines instances for suitable libraries in the
standard library in order to have no dependencies. The instances for
vector and unordered-containers can be found here:

[listing-unordered-containers](https://github.com/nanonaren/listing-unordered-containers)

[listing-vector](https://github.com/nanonaren/listing-vector)

Examples
--------

```haskell
import Data.Listing
import Data.Set (Set)
import Data.Map.Lazy (Map)

lst :: [Int]
lst = [1..10]

someMap :: Map String Int
someMap = fromList [("a",1),("b",2)]

someSet :: Set Int
someSet = fromList [1..10]

-- | Looking up values is now convinient
total = lst ! 1 + someMap ! "a"

-- | checking sizes
sizes = size lst + size someMap

-- | dealing with Maybe is also unified
--   i.e. head replaces fromJust
maybeEx = head (Just 10) == 10

-- | extracting values from map also unified
--   gets the min. I have plans to put "last" in the interface
--   that would get the max.
extract = head someMap == ("a",1)

-- | checking membership in sets
check = someSet ! 3 == True &&
        someSet ! 11 == False

```