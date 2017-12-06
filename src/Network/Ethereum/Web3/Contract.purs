module Network.Ethereum.Web3.Contract where

import Prelude

import Control.Monad.Aff (Fiber, delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Parallel.Class (parallel, sequential)
import Data.Array (notElem, catMaybes)
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic, Constructor)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3.Api (eth_call, eth_getFilterChanges, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, forkWeb3')
import Network.Ethereum.Web3.Solidity (class ArgsToRowListProxy, class DecodeEvent, class GenericABIDecode, class GenericABIEncode, class ToRecordFields, decodeEvent, genericABIEncode, genericFromData, genericToRecordFields)
import Network.Ethereum.Web3.Types (class EtherUnit, Address, CallMode, Change, ETH, Filter, FilterId, HexString, Web3, _data, _from, _gas, _to, _value, convert, defaultTransactionOptions, hexadecimal, parseBigNumber, toSelector)
import Type.Proxy (Proxy(..))
import Type.Row (class ListToRow)


--------------------------------------------------------------------------------
-- * Events
--------------------------------------------------------------------------------

-- | Represents a flag to continue or discontinue listening to the filter
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener

derive instance genericEventAction :: Generic EventAction _

instance showEventAction :: Show EventAction where
  show = genericShow

instance eqEventAction :: Eq EventAction where
  eq = genericEq

class EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter


-- | Start listening to events eminating from the given address and caught by the filter,
-- | using the handler to process the data and decide whether to continue
event :: forall p e a i ni.
          IsAsyncProvider p
       => DecodeEvent i ni a
       => EventFilter a
       => Address
       -> (a -> ReaderT Change (Web3 p e) EventAction)
       -> Web3 p e (Fiber (eth :: ETH | e) Unit)
event addr handler = do
    fid <- eth_newFilter (eventFilter (Proxy :: Proxy a) addr)
    forkWeb3' (Proxy :: Proxy p) $ do
      loop fid
      _ <- eth_uninstallFilter fid
      pure unit
  where
    loop :: FilterId -> Web3 p e Unit
    loop fltr = do
      _ <- liftAff $ delay (Milliseconds 1000.0)
      changes <- eth_getFilterChanges fltr
      acts <- for (catMaybes $ map pairChange changes) $ \(Tuple changeWithMeta changeEvent) -> do
        runReaderT (handler changeEvent) changeWithMeta
      when (TerminateEvent `notElem` acts) $ loop fltr
    pairChange :: Change -> Maybe (Tuple Change a)
    pairChange rawChange = do
      change <- decodeEvent rawChange
      pure (Tuple rawChange change)

-- | Same as 'event', but execute the handler in parallel over the captured events in a given block.
eventPar :: forall p e a i ni.
          IsAsyncProvider p
       => DecodeEvent i ni a
       => EventFilter a
       => Address
       -> (a -> ReaderT Change (Web3 p e) EventAction)
       -> Web3 p e (Fiber (eth :: ETH | e) Unit)
eventPar addr handler = do
    fid <- eth_newFilter (eventFilter (Proxy :: Proxy a) addr)
    forkWeb3' (Proxy :: Proxy p) $ do
      loop fid
      _ <- eth_uninstallFilter fid
      pure unit
  where
    loop :: FilterId -> Web3 p e Unit
    loop fltr = do
      _ <- liftAff $ delay (Milliseconds 1000.0)
      changes <- eth_getFilterChanges fltr
      acts <- sequential $ for (catMaybes $ map pairChange changes) $ \(Tuple changeWithMeta changeEvent) ->
        runReaderT (parallel $ handler changeEvent) changeWithMeta
      when (TerminateEvent `notElem` acts) $ loop fltr
    pairChange :: Change -> Maybe (Tuple Change a)
    pairChange rawChange = do
      change <- decodeEvent rawChange
      pure (Tuple rawChange change)

--------------------------------------------------------------------------------
-- * Methods
--------------------------------------------------------------------------------

-- | Class paramaterized by values which are ABIEncodable, allowing the templating of
-- | of a transaction with this value as the payload.
class TxMethod (selector :: Symbol) a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall p e u.
              IsAsyncProvider p
           => IsSymbol selector
           => EtherUnit u
           => Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> u
           -- ^ paymentValue
           -> Tagged (SProxy selector) a
           -- ^ Method data
           -> Web3 p e HexString
           -- ^ 'Web3' wrapped tx hash

class CallMethod (selector :: Symbol) a b | selector a -> b where
    -- | Constant call given contract 'Address' in mode and given input data
    callMethod :: forall p e.
                  IsAsyncProvider p
               => Address
               -- ^ Contract address
               -> Maybe Address
               -- from address
               -> CallMode
               -- ^ State mode for constant call (latest or pending)
               -> Tagged (SProxy selector) a
               -- ^ Method data
               -> Web3 p e b
               -- ^ 'Web3' wrapped result

instance txmethodAbiEncode :: (Generic a rep, GenericABIEncode rep) => TxMethod s a where
  sendTx = _sendTransaction

instance callmethodAbiEncode :: (Generic a arep, GenericABIEncode arep, Generic b brep, GenericABIDecode brep, IsSymbol s) => CallMethod s a b where
  callMethod = _callMethod

_sendTransaction :: forall p a rep e u selector .
                    IsAsyncProvider p
                 => IsSymbol selector
                 => Generic a rep
                 => GenericABIEncode rep
                 => EtherUnit u
                 => Maybe Address
                 -> Address
                 -> u
                 -> Tagged (SProxy selector) a
                 -> Web3 p e HexString
_sendTransaction mto f val dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    eth_sendTransaction <<< txdata $ sel <> (genericABIEncode <<< untagged $ dat)
  where
    defaultGas = parseBigNumber hexadecimal "0x2dc2dc"
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ Just f
                                # _data .~ Just d
                                # _value .~ Just (convert val)
                                # _gas .~ defaultGas

_callMethod :: forall p a arep b brep e selector .
               IsAsyncProvider p
            => IsSymbol selector
            => Generic a arep
            => GenericABIEncode arep
            => Generic b brep
            => GenericABIDecode brep
            => Address
            -> Maybe Address
            -> CallMode
            -> Tagged (SProxy selector) a
            -> Web3 p e b
_callMethod t mf cm dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    res <- eth_call (txdata $ sel <> (genericABIEncode <<< untagged $ dat)) cm
    case genericFromData res of
        Nothing -> throwError <<< error $ "Unable to parse result"
        Just x -> pure x
  where
    txdata d  =
      defaultTransactionOptions # _to .~ Just t
                                # _from .~ mf
                                # _data .~ Just d

class Call (selector :: Symbol) a b fields where
    call :: forall p e.
            IsAsyncProvider p
         => Proxy b
         -> Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> Tagged (SProxy selector) a
         -- ^ Method data
         -> Web3 p e (Record fields)
         -- ^ 'Web3' wrapped result

instance defaultCall :: ( CallMethod selector a b
                        , ToRecordFields args fields l
                        , Generic b (Constructor name args)
                        , ArgsToRowListProxy args l
                        , ListToRow l fields
                        ) => Call selector a b fields where
  call _ to mfrom cm pl = genericToRecordFields <$> callMethod to mfrom cm pl
