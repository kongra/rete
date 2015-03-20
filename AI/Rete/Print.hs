{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Print
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-11-14
-- Reworked    : 2015-02-09, 2015-03-16
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- Textual visualization of Rete network and data.
------------------------------------------------------------------------
module AI.Rete.Print
    (
      -- * Print methods
      toShowS
    , toString

      -- * The 'Depth' constraints of the tree traversal process
    , Depth
    , depth
    , boundless

      -- * 'Switch'es
    , Switch
    , with, no, clear

      -- * Predefined 'Switch'es
    , withNet
    , noNet
    , withData
    , noData

      -- * Actions and related utils
    , traceTokAction
    , traceVarAction

      -- * 'Flag's (detailed)
    , Flag (..)
    )
    where

import           AI.Rete.Data
import           AI.Rete.Flow
import           AI.Rete.Net (traceAction, valE)
import           AI.Rete.State
import           Control.Monad (liftM)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.List (intersperse)
import           Data.Maybe (catMaybes)
import           Data.Tree.Print
import           Kask.Control.Lens
import           Kask.Control.Monad (toListM)
import           Kask.Data.Function (compose, rcompose)

-- CONFIGURATION

-- | A Boolean (semanticaly) configuration option for the printing
-- process.
data Flag =
  NetEmph | DataEmph

  | AmemFields | AmemWmes | AmemWmesCount | AmemSuccessors

  | NodeChildren

  | BmemToks | BmemToksCount

  | JoinTests  | JoinAmems

  | Bindings | PredsCount deriving (Show, Eq)

flagCode :: Flag -> Int
flagCode NetEmph        = 1
flagCode DataEmph       = 2
flagCode AmemFields     = 3
flagCode AmemWmes       = 4
flagCode AmemWmesCount  = 5
flagCode AmemSuccessors = 6
flagCode NodeChildren   = 7
flagCode BmemToks       = 8
flagCode BmemToksCount  = 9
flagCode JoinTests      = 10
flagCode JoinAmems      = 11
flagCode Bindings       = 12
flagCode PredsCount     = 13

instance Hashable Flag where
  hashWithSalt salt flag = salt `hashWithSalt` flagCode flag

-- | A set of 'Flags's.
type Flags = Set.HashSet Flag

-- | A switch to turn the 'Flag's on/off.
type Switch = Flags -> Flags

-- | Creates a 'Switch' that turns the 'Flag' on.
with :: Flag -> Switch
with = Set.insert

-- | Creates a 'Switch' that turns the 'Flag' off.
no :: Flag -> Switch
no = Set.delete

-- | Creates a 'Switch' that turns all flags off.
clear :: Switch
clear _ = noFlags

-- | Asks whether the 'Flag' is on in 'Flags'.
is :: Flag -> Flags -> Bool
is = Set.member

-- | A set of 'Flag's with all 'Flag's turned off.
noFlags :: Flags
noFlags = Set.empty

-- PREDEFINED Switch CONFIGURATIONS

dataFlags :: [Flag]
dataFlags =  [ AmemWmes
             , AmemWmesCount
             , BmemToks
             , BmemToksCount ]

netFlags :: [Flag]
netFlags =  [ AmemFields
            , AmemSuccessors
            , NodeChildren
            , JoinTests
            , JoinAmems
            , Bindings
            , PredsCount ]

-- | A 'Switch' that turns data presentation off.
noData :: Switch
noData = compose (map no dataFlags)

-- | A 'Switch' that turns data presentation on.
withData :: Switch
withData = compose (map with dataFlags)

-- | A 'Switch' that turns network presentation off.
noNet :: Switch
noNet = compose (map no netFlags)

-- | A 'Switch' that turns network presentation on.
withNet :: Switch
withNet = compose (map with netFlags)

-- DEFENDING AGAINST CYCLES

data Visited = Visited { visitedAmems :: !(Set.HashSet Amem)
                       , visitedBmems :: !(Set.HashSet Bmem)
                       , visitedJoins :: !(Set.HashSet Join)
                       , visitedProds :: !(Set.HashSet Prod) }

cleanVisited :: Visited
cleanVisited =  Visited { visitedAmems = Set.empty
                        , visitedBmems = Set.empty
                        , visitedJoins = Set.empty
                        , visitedProds = Set.empty }

class Visitable a where
  visiting :: a -> Visited -> Visited
  visited  :: a -> Visited -> Bool

instance Visitable Amem where
  visiting amem vs = vs { visitedAmems = Set.insert amem (visitedAmems vs) }
  visited  amem vs = Set.member amem (visitedAmems vs)

instance Visitable Bmem where
  visiting bmem vs = vs { visitedBmems = Set.insert bmem (visitedBmems vs) }
  visited  bmem vs = Set.member bmem (visitedBmems vs)

instance Visitable Join where
  visiting join vs = vs { visitedJoins = Set.insert join (visitedJoins vs) }
  visited  join vs = Set.member join (visitedJoins vs)

instance Visitable Prod where
  visiting prod vs = vs { visitedProds = Set.insert prod (visitedProds vs) }
  visited  prod vs = Set.member prod (visitedProds vs)

withEllipsis :: Bool -> ShowS -> ReteM ShowS
withEllipsis False s = return s
withEllipsis True  s = return (compose [s, showString " ..."])

withEllipsisM :: Bool -> ReteM ShowS -> ReteM ShowS
withEllipsisM v s = s >>= withEllipsis v

whenNot :: Bool -> ReteM [Vn] -> ReteM [Vn]
whenNot True  _   = return []
whenNot False vns = vns

-- Vns (VISUALIZATION NODEs)

type VnShow = Flags -> Visited -> ReteM ShowS
type VnAdjs = Flags -> Visited -> ReteM [Vn]

-- | Represents types whose values are convertible to Vn.
class Vnable a where
  toVnShow :: a -> VnShow
  toVnAdjs :: a -> VnAdjs

-- | Visualization node.
data Vn = Vn { vnShowM   :: !VnShow
             , vnAdjs    :: !VnAdjs
             , vnVisited :: !Visited }

-- | Converts the passed object to a Vn.
toVn :: Vnable a => Visited -> a -> Vn
toVn vs x = Vn { vnShowM   = toVnShow x
               , vnAdjs    = toVnAdjs x
               , vnVisited = vs }

instance ShowM (S.State ReteState) Flags Vn where
  showM flags Vn { vnShowM = f, vnVisited = vs } = f flags vs

-- SPECIFIC Vns

-- | Creates a Vn that has no adjs - thus is a leaf.
leafVn :: Visited -> VnShow -> Vn
leafVn vs show' = Vn { vnShowM   = show'
                     , vnAdjs    = \_ _ -> return []
                     , vnVisited = vs }

-- | Creates a label Vn with passed adjs.
labelVn :: Visited -> ShowS -> [Vn] -> Vn
labelVn vs label adjs' = Vn { vnShowM   = \_ _ -> return label
                            , vnAdjs    = \_ _ -> return adjs'
                            , vnVisited = vs }

-- | Creates a Vn that represents a label with a sequence of leaf
-- subnodes (Vns).
labeledLeavesVn :: Visited -> ShowS -> [VnShow] -> Vn
labeledLeavesVn vs label shows' = labelVn vs label (map (leafVn vs) shows')

-- | Converts the monadic foldable into a sequence of Vns. All in the
-- m monad.
toVnsM :: (Monad m, Foldable f, Vnable a) => Visited -> m (f a) -> m [Vn]
toVnsM vs = liftM (map (toVn vs)) . toListM

-- | Works like toVnsM, but returns a list of VnShows instead of Vns.
toVnShowsM :: (Monad m, Foldable f, Vnable a) => m (f a) -> m [VnShow]
toVnShowsM = liftM (map toVnShow) . toListM

type OptLabelVn = (Monad m, Foldable f, Vnable a)
               => Bool -> String -> Visited -> m (f a) -> m (Maybe Vn)

-- | Returns an optional Vn that represents a label with a
-- sub-sequence of adjs (Vns).
optLabeledVn :: OptLabelVn
optLabeledVn False _     _  _  = return Nothing
optLabeledVn True  label vs adjs' = do
  vns <- toVnsM vs adjs'
  if null vns
     then return Nothing
     else return (Just (labelVn vs (showString label) vns))

-- | Returns an optional Vn that represents a label with a
-- sub-sequence of leaf adjs (Vns).
optLabeledLeavesVn :: OptLabelVn
optLabeledLeavesVn False _     _  _  = return Nothing
optLabeledLeavesVn True  label vs xs = do
  shows' <- toVnShowsM xs
  if null shows'
     then return Nothing
     else return (Just (labeledLeavesVn vs (showString label) shows'))

-- | Strips off Nothings out of the input collection of Maybe Vns.
optVns :: Monad m => [Maybe Vn] -> m [Vn]
optVns = return . catMaybes

-- | Creates a node with an emphasis on the network structure.
netVn :: Flags -> OptLabelVn
netVn flags = if is NetEmph flags then optLabeledVn else optLabeledLeavesVn

-- | Creates a node with an emphasis on the data.
datVn :: Flags -> OptLabelVn
datVn flags = if is DataEmph flags then optLabeledVn else optLabeledLeavesVn

-- CONFIGURATION

type VConf = Conf (S.State ReteState) ShowS Flags Vn

conf :: VConf
conf = Conf { impl     = reteMImpl
            , adjs     = \flags Vn { vnAdjs = f, vnVisited = vs } -> f flags vs
            , maxDepth = Nothing
            , opts     = noFlags }

-- | A specifier of depth of the treePrint process.
type Depth = VConf -> VConf

-- | Sets the maxDepth of a configuration to the specified value.
depth :: Int -> Depth
depth d c = c { maxDepth = Just d }

-- | Unlimits the maxDepth of a configuration.
boundless :: Depth
boundless c = c { maxDepth = Nothing }

applySwitch :: Switch -> VConf -> VConf
applySwitch switch c@Conf { opts = opts' } = c { opts = switch opts' }

-- ReteM IMPL

reteMImpl :: Impl (S.State ReteState) ShowS
reteMImpl = str

-- Wmes VIS.

instance Vnable Wme where
  toVnShow wme _ _ = return (shows wme)
  toVnAdjs _   _ _ = return []

-- Toks VIS.

instance Vnable Tok where
  toVnShow tok _ _ = return (showTok tok)
  toVnAdjs _   _ _ = return []

showTok :: Tok -> ShowS
showTok []  = showString "{}"
showTok tok =
  compose [ showString "{"
          , tokWmeS
          , showString "}"] where

    tokWmeS = rcompose (intersperse colon (map shows (reverse tok)))
    colon   = showString ","

-- AMEMS VIS.

instance Vnable Amem where
  toVnAdjs = amemAdjs
  toVnShow = showAmem

showAmem :: Amem -> Flags -> Visited -> ReteM ShowS
showAmem amem flags vs = do
  let (Obj  o) = amemObj    amem
      (Attr a) = amemAttr   amem
      (Val  v) = amemVal    amem
      repr     = if is AmemFields flags
                   then compose [ shows amem , showString " ("
                                , shows o    , showString ","
                                , shows a    , showString ","
                                , shows v    , showString ")"]
                   else shows amem
  withEllipsisM (visited amem vs) $
    if is AmemWmesCount flags
      then (do wmes <- liftM (view amemWmes) (viewS amem)
               return $ compose [ repr
                                , showString ", "
                                , shows (length wmes)
                                , showString " wmes"])
      else return repr

amemAdjs :: Amem -> Flags -> Visited -> ReteM [Vn]
amemAdjs amem flags vs = whenNot (visited amem vs) $ do
  let vs'   = visiting            amem vs
  amemState <- viewS              amem
  let succs = view amemSuccessors amemState
      wmes  = view amemWmes       amemState

  succVn <- netVn flags (is AmemSuccessors flags) "succs" vs' (return succs)
  wmesVn <- datVn flags (is AmemWmes       flags) "wmes"  vs' (return wmes )
  optVns [succVn, wmesVn]

-- BMEM VIS.

instance Vnable Bmem where
  toVnAdjs = bmemAdjs
  toVnShow = showBmem

showBmem :: Bmem -> Flags -> Visited -> ReteM ShowS
showBmem bmem flags vs = withEllipsisM (visited bmem vs) $
  if is BmemToksCount flags
    then (do toks <- liftM (view bmemToks) (viewS bmem)
             return $ compose [ shows bmem
                              , showString ", "
                              , shows (length toks)
                              , showString " toks"])
    else return (shows bmem)

bmemAdjs :: Bmem -> Flags -> Visited -> ReteM [Vn]
bmemAdjs bmem flags vs = whenNot (visited bmem vs) $ do
  let vs'    = visiting          bmem vs
  bmemState <- viewS             bmem
  let chld   = view bmemChildren bmemState
      toks   = view bmemToks     bmemState

  childrenVn <- netVn flags (is NodeChildren flags) "children" vs' (return chld)
  toksVn     <- datVn flags (is BmemToks     flags) "toks"     vs' (return toks)
  optVns [childrenVn, toksVn]

-- JOIN VIS.

instance Vnable Join where
  toVnAdjs = joinAdjs
  toVnShow = showJoin

showJoin :: Join -> Flags -> Visited -> ReteM ShowS
showJoin join _ vs = withEllipsisM (visited join vs) $ return (shows join)

joinAdjs :: Join -> Flags -> Visited -> ReteM [Vn]
joinAdjs join flags vs = whenNot (visited join vs) $ do
  let vs' = visiting join vs
  joinState <- viewS join
  let bmem = case view joinChildBmem joinState of
        Nothing -> []
        Just b  -> [b]

      prods = view joinChildProds joinState
      tests = joinTests join
      amem  = [joinAmem join]

  amemVn  <- netVn flags (is JoinAmems    flags) "amem"          vs' (return amem)
  testsVn <- netVn flags (is JoinTests    flags) "tests"         vs' (return tests)
  bmemVn  <- netVn flags (is NodeChildren flags) "(child) bmem"  vs' (return bmem)
  prodsVn <- netVn flags (is NodeChildren flags) "(child) prods" vs' (return prods)

  optVns [amemVn, testsVn, bmemVn, prodsVn]

-- PROD VIS.

instance Vnable Prod where
  toVnAdjs = prodAdjs
  toVnShow = showProd

showProd :: Prod -> Flags -> Visited -> ReteM ShowS
showProd prod flags vs = withEllipsisM (visited prod vs) $
  if is PredsCount flags
    then return $ compose [ shows prod
                          , showString ", "
                          , shows (length (prodPreds prod))
                          , showString " preds"]
    else return (shows prod)

prodAdjs :: Prod -> Flags -> Visited -> ReteM [Vn]
prodAdjs prod flags vs = whenNot (visited prod vs) $ do
  let vs' = visiting prod vs
  bindingsVn <- netVn flags (is Bindings flags) "bindings" vs'
                (varlocs (prodBindings prod))
  optVns [bindingsVn]

data VLoc = VLoc !Variable !Int !Field

varlocs :: Bindings -> ReteM [VLoc]
varlocs = return . map vbinding2VLoc . Map.toList
  where vbinding2VLoc (s, Location d f) = VLoc s d f

instance Vnable VLoc where
  toVnAdjs _ _ _ = return []
  toVnShow       = showVLoc

showVLoc :: VLoc -> Flags -> Visited -> ReteM ShowS
showVLoc (VLoc s f d) _ _ =
  return (compose [ shows s, showString " → "
                  , shows d, showString ",", shows f])

-- JoinTest VIS.

instance Vnable JoinTest where
  toVnAdjs _ _ _ = return []
  toVnShow       = showJoinTest

showJoinTest :: JoinTest -> Flags -> Visited -> ReteM ShowS
showJoinTest
  JoinTest { joinField1   = f1
           , joinField2   = f2
           , joinDistance = d } _ _ =
    return (compose [ showString "⟨"
                    , shows f1, showString ","
                    , shows d,  showString ","
                    , shows f2
                    , showString "⟩"])

-- RETE VIS.

instance Vnable Rete where  -- Simply visualize dtn
  toVnAdjs _ = toVnAdjs dtn
  toVnShow _ = toVnShow dtn

-- PRINT IMPLEMENTATION

-- | Converts the selected object to a tree representation (expressed
-- in ShowS).
toShowS :: Vnable a => Depth -> Switch -> a -> ReteM ShowS
toShowS d switch obj = printTree (switches conf) (toVn cleanVisited obj)
  where switches = d . applySwitch switch

-- | Works like toShowS, but returns String instead of ShowS
toString :: Vnable a => Depth -> Switch -> a -> ReteM String
toString d switch = liftM evalShowS . toShowS d switch
  where evalShowS s = s ""

-- ACTIONS AND RELATED UTILS

-- | Action that traces the matching token (prefixed).
traceTokAction :: String -> Action
traceTokAction prefix = traceAction dumpTok
  where dumpTok Actx { actxTok = tok } =
          return $ compose [showString prefix, showTok tok] ""

-- | Action that traces the value of a Var (prefixed).
traceVarAction :: String -> Var -> Action
traceVarAction prefix v = traceAction dumpVarVal
  where
    dumpVarVal actx = do
      c <- valE v actx
      return (prefix ++ show c)
