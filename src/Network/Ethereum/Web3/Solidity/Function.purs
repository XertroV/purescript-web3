module Network.Ethereum.Web3.Solidity.Function where


import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (Argument(..), NoArguments(..), Product(..))
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class RowLacks, class RowToList, RLProxy(..), kind RowList, Cons, Nil)

{-

want something like

recordToTuple :: Proxy ["to", "amount"] -> {to :: Address, amount :: Amount} -> Tuple2 (Tagged "to" Address) (Tagged "amount" UInt)


-}


