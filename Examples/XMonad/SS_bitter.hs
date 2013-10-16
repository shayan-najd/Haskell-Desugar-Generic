{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoMonomorphismRestriction, MonoLocalBinds #-}
module XMonad.StackSet
       (StackSet(..), Workspace(..), Screen(..), Stack(..),
        RationalRect(..), new, view, greedyView, lookupWorkspace, screens,
        workspaces, allWindows, currentTag, peek, index, integrate,
        integrate', differentiate, focusUp, focusDown, focusUp',
        focusDown', focusMaster, focusWindow, tagMember, renameTag,
        ensureTags, member, findTag, mapWorkspace, mapLayout, insertUp,
        delete, delete', filter, swapUp, swapDown, swapMaster, shiftMaster,
        modify, modify', float, sink, shift, shiftWin, abort)
       where
import Prelude
       (Ord, Eq, Rational, Show, Read, String, Integral,
        Bool(True, False), Maybe(Just, Nothing), maybe, reverse, map, elem,
        return, otherwise, concatMap, not, null, length, zip3, until, any,
        error, (.), (/=), (==), (<=), ($), (++), (>>=), (&&),enumFrom)
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import qualified Data.List as L
       (deleteBy, find, splitAt, filter, nub)
import Data.List ((\\))
import qualified Data.Map as M (Map, insert, delete, empty)
import Control.Monad (fail,(>>),guard) 
  
data StackSet i l a sid sd = StackSet{current ::
                                      !(Screen i l a sid sd),
                                      visible :: [Screen i l a sid sd], hidden :: [Workspace i l a],
                                      floating :: M.Map a RationalRect}
                           deriving (Show, Read, Eq)
 
data Screen i l a sid sd = Screen{workspace :: !(Workspace i l a),
                                  screen :: !sid, screenDetail :: !sd}
                         deriving (Show, Read, Eq)
 
data Workspace i l a = Workspace{tag :: !i, layout :: l,
                                 stack :: Maybe (Stack a)}
                     deriving (Show, Read, Eq)
 
data RationalRect = RationalRect Rational Rational Rational
                                 Rational
                  deriving (Show, Read, Eq)
 
data Stack a = Stack{focus :: !a, up :: [a], down :: [a]}
             deriving (Show, Read, Eq)
 
abort :: String -> a
abort
  = \ _x7 ->
      case (_x7) of
          (x) -> ($) error ((++) "xmonad: StackSet: " x)
 
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new
  = \ _x8 _x9 _x10 ->
      case (_x8, _x9, _x10) of
          (l, wids, m) | (&&) (not (null wids))
                           ((&&) ((<=) (length m) (length wids)) (not (null m)))
                         -> StackSet cur visi unseen M.empty
            where (,) seen unseen
                    = ($) (L.splitAt (length m))
                        (map (\ i -> Workspace i l Nothing) wids)
                  ((:) cur visi)
                    = (>>=) (zip3 seen (enumFrom 0) m)
                        (\ _x0 ->
                           case _x0 of
                               (,,) i s sd -> return (Screen i s sd)
                               _ -> fail "...")
          (_, _, _) -> abort "non-positive argument to StackSet.new"
 
view ::
       (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view
  = \ _x11 _x12 ->
      case (_x11, _x12) of
          (i, s) | (==) i (currentTag s) -> s
                 | Just x <- L.find ((.) (\ _x2 -> (==) i _x2) ((.) tag workspace))
                               (visible s)
                   ->
                   s{current = x,
                     visible =
                       (:) (current s) (L.deleteBy (equating screen) x (visible s))}
                 | Just x <- L.find ((.) (\ _x3 -> (==) i _x3) tag) (hidden s) ->
                   s{current = (current s){workspace = x},
                     hidden =
                       (:) (workspace (current s))
                         (L.deleteBy (equating tag) x (hidden s))}
                 | otherwise -> s
            where equating
                    = \ _x13 ->
                        case (_x13) of
                            (f) -> \ x y -> (==) (f x) (f y)
 
greedyView ::
             (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView
  = \ _x14 _x15 ->
      case (_x14, _x15) of
          (w, ws) | any wTag (hidden ws) -> view w ws
                  | (Just s) <- L.find ((.) wTag workspace) (visible ws) ->
                    ws{current = (current ws){workspace = workspace s},
                       visible =
                         (:) s{workspace = workspace (current ws)}
                           (L.filter ((.) not ((.) wTag workspace)) (visible ws))}
                  | otherwise -> ws
            where wTag = (.) (\ _x4 -> (==) w _x4) tag
 
lookupWorkspace :: (Eq s) => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace
  = \ _x16 _x17 ->
      case (_x16, _x17) of
          (sc, w) -> listToMaybe
                       ((>>=) ((:) (current w) (visible w))
                          (\ _x1 ->
                             case _x1 of
                                 Screen i s _ -> (>>) (guard ((==) s sc)) (return (tag i))
                                 _ -> fail "..."))
 
with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with
  = \ _x18 _x19 ->
      case (_x18, _x19) of
          (dflt, f) -> (.) (maybe dflt f) ((.) stack ((.) workspace current))
 
modify ::
       Maybe (Stack a) ->
         (Stack a -> Maybe (Stack a)) ->
           StackSet i l a s sd -> StackSet i l a s sd
modify
  = \ _x20 _x21 _x22 ->
      case (_x20, _x21, _x22) of
          (d, f, s) -> s{current =
                           (current s){workspace =
                                         (workspace (current s)){stack = with d f s}}}
 
modify' ::
        (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify'
  = \ _x23 ->
      case (_x23) of
          (f) -> modify Nothing ((.) Just f)
 
peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing ((.) return focus)
 
integrate :: Stack a -> [a]
integrate
  = \ _x24 ->
      case (_x24) of
          ((Stack x l r)) -> (++) (reverse l) ((:) x r)
 
integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate
 
differentiate :: [a] -> Maybe (Stack a)
differentiate
  = \ _x25 ->
      case (_x25) of
          ([]) -> Nothing
          (((:) x xs)) -> ($) Just (Stack x [] xs)
 
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter
  = \ _x26 _x27 ->
      case (_x26, _x27) of
          (p, (Stack f ls rs)) -> case L.filter p ((:) f rs) of
                                      (:) f' rs' -> ($) Just (Stack f' (L.filter p ls) rs')
                                      [] -> case L.filter p ls of
                                                (:) f' ls' -> ($) Just (Stack f' ls' [])
                                                [] -> Nothing
 
index :: StackSet i l a s sd -> [a]
index = with [] integrate
 
focusUp, focusDown, swapUp, swapDown ::
         StackSet i l a s sd -> StackSet i l a s sd
focusUp = modify' focusUp'
focusDown = modify' focusDown'
swapUp = modify' swapUp'
swapDown = modify' ((.) reverseStack ((.) swapUp' reverseStack))
 
focusUp', focusDown' :: Stack a -> Stack a
focusUp'
  = \ _x28 ->
      case (_x28) of
          ((Stack t ((:) l ls) rs)) -> Stack l ls ((:) t rs)
          ((Stack t [] rs)) -> Stack x xs []
            where ((:) x xs) = reverse ((:) t rs)
focusDown' = (.) reverseStack ((.) focusUp' reverseStack)
 
swapUp' :: Stack a -> Stack a
swapUp'
  = \ _x29 ->
      case (_x29) of
          ((Stack t ((:) l ls) rs)) -> Stack t ls ((:) l rs)
          ((Stack t [] rs)) -> Stack t (reverse rs) []
 
reverseStack :: Stack a -> Stack a
reverseStack
  = \ _x30 ->
      case (_x30) of
          ((Stack t ls rs)) -> Stack t rs ls
 
focusWindow ::
              (Eq s, Eq a, Eq i) =>
              a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow
  = \ _x31 _x32 ->
      case (_x31, _x32) of
          (w, s) | (==) (Just w) (peek s) -> s
                 | otherwise ->
                   ($) (fromMaybe s)
                     ((>>=) (findTag w s)
                        (\ n ->
                           ($) return
                             (until ((.) (\ _x5 -> (==) (Just w) _x5) peek) focusUp
                                (view n s))))
 
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens
  = \ _x33 ->
      case (_x33) of
          (s) -> (:) (current s) (visible s)
 
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces
  = \ _x34 ->
      case (_x34) of
          (s) -> (:) (workspace (current s))
                   ((++) (map workspace (visible s)) (hidden s))
 
allWindows :: (Eq a) => StackSet i l a s sd -> [a]
allWindows
  = (.) L.nub ((.) (concatMap ((.) integrate' stack)) workspaces)
 
currentTag :: StackSet i l a s sd -> i
currentTag = (.) tag ((.) workspace current)
 
tagMember :: (Eq i) => i -> StackSet i l a s sd -> Bool
tagMember
  = \ _x35 ->
      case (_x35) of
          (t) -> (.) (elem t) ((.) (map tag) workspaces)
 
renameTag ::
            (Eq i) => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag
  = \ _x36 _x37 ->
      case (_x36, _x37) of
          (o, n) -> mapWorkspace rename
            where rename
                    = \ _x38 ->
                        case (_x38) of
                            (w) -> case (==) (tag w) o of
                                       True -> w{tag = n}
                                       False -> w
 
ensureTags ::
             (Eq i) => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags
  = \ _x39 _x40 _x41 ->
      case (_x39, _x40, _x41) of
          (l, allt, st) -> et allt ((\\) (map tag (workspaces st)) allt) st
            where et
                    = \ _x42 _x43 _x44 ->
                        case (_x42, _x43, _x44) of
                            ([], _, s) -> s
                            (((:) i is), rn, s) | tagMember i s -> et is rn s
                            (((:) i is), [], s) -> et is []
                                                     (s{hidden =
                                                          (:) (Workspace i l Nothing) (hidden s)})
                            (((:) i is), ((:) r rs), s) -> ($) (et is rs) (renameTag r i s)
 
mapWorkspace ::
             (Workspace i l a -> Workspace i l a) ->
               StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace
  = \ _x45 _x46 ->
      case (_x45, _x46) of
          (f, s) -> s{current = updScr (current s),
                      visible = map updScr (visible s), hidden = map f (hidden s)}
            where updScr
                    = \ _x47 ->
                        case (_x47) of
                            (scr) -> scr{workspace = f (workspace scr)}
 
mapLayout ::
          (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout
  = \ _x48 _x49 ->
      case (_x48, _x49) of
          (f, (StackSet v vs hs m)) -> StackSet (fScreen v) (map fScreen vs)
                                         (map fWorkspace hs)
                                         m
            where fScreen
                    = \ _x50 ->
                        case (_x50) of
                            ((Screen ws s sd)) -> Screen (fWorkspace ws) s sd
                  fWorkspace
                    = \ _x51 ->
                        case (_x51) of
                            ((Workspace t l s)) -> Workspace t (f l) s
 
member :: (Eq a) => a -> StackSet i l a s sd -> Bool
member
  = \ _x52 _x53 ->
      case (_x52, _x53) of
          (a, s) -> isJust (findTag a s)
 
findTag :: (Eq a) => a -> StackSet i l a s sd -> Maybe i
findTag
  = \ _x54 _x55 ->
      case (_x54, _x55) of
          (a, s) -> listToMaybe
                      ((>>=) (workspaces s)
                         (\ w -> (>>) (guard (has a (stack w))) (return (tag w))))
            where has
                    = \ _x56 _x57 ->
                        case (_x56, _x57) of
                            (_, Nothing) -> False
                            (x, (Just (Stack t l r))) -> elem x ((:) t ((++) l r))
 
insertUp ::
           (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp
  = \ _x58 _x59 ->
      case (_x58, _x59) of
          (a, s) -> case member a s of
                        True -> s
                        False -> insert
            where insert
                    = modify (($) Just (Stack a [] []))
                        (\ (Stack t l r) -> ($) Just (Stack a l ((:) t r)))
                        s
 
delete ::
         (Ord a, Eq s) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete
  = \ _x60 ->
      case (_x60) of
          (w) -> (.) (sink w) (delete' w)
 
delete' ::
          (Eq a, Eq s) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete'
  = \ _x61 _x62 ->
      case (_x61, _x62) of
          (w, s) -> s{current = removeFromScreen (current s),
                      visible = map removeFromScreen (visible s),
                      hidden = map removeFromWorkspace (hidden s)}
            where removeFromWorkspace
                    = \ _x63 ->
                        case (_x63) of
                            (ws) -> ws{stack = (>>=) (stack ws) (filter (\ _x6 -> (/=) _x6 w))}
                  removeFromScreen
                    = \ _x64 ->
                        case (_x64) of
                            (scr) -> scr{workspace = removeFromWorkspace (workspace scr)}
 
float ::
        (Ord a) =>
        a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float
  = \ _x65 _x66 _x67 ->
      case (_x65, _x66, _x67) of
          (w, r, s) -> s{floating = M.insert w r (floating s)}
 
sink :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
sink
  = \ _x68 _x69 ->
      case (_x68, _x69) of
          (w, s) -> s{floating = M.delete w (floating s)}
 
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster
  = ($) modify'
      (\ c ->
         case c of
             Stack _ [] _ -> c
             Stack t ls rs -> Stack t [] ((++) xs ((:) x rs))
               where ((:) x xs) = reverse ls)
 
shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster
  = ($) modify'
      (\ c ->
         case c of
             Stack _ [] _ -> c
             Stack t ls rs -> Stack t [] ((++) (reverse ls) rs))
 
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster
  = ($) modify'
      (\ c ->
         case c of
             Stack _ [] _ -> c
             Stack t ls rs -> Stack x [] ((++) xs ((:) t rs))
               where ((:) x xs) = reverse ls)
 
shift ::
        (Ord a, Eq s, Eq i) =>
        i -> StackSet i l a s sd -> StackSet i l a s sd
shift
  = \ _x70 _x71 ->
      case (_x70, _x71) of
          (n, s) -> maybe s (\ w -> shiftWin n w s) (peek s)
 
shiftWin ::
           (Ord a, Eq a, Eq s, Eq i) =>
           i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin
  = \ _x72 _x73 _x74 ->
      case (_x72, _x73, _x74) of
          (n, w, s) -> case findTag w s of
                           Just from | (&&) (tagMember n s) ((/=) n from) -> go from s
                           _ -> s
            where go
                    = \ _x75 ->
                        case (_x75) of
                            (from) -> (.) (onWorkspace n (insertUp w))
                                        (onWorkspace from (delete' w))
 
onWorkspace ::
              (Eq i, Eq s) =>
              i ->
                (StackSet i l a s sd -> StackSet i l a s sd) ->
                  (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace
  = \ _x76 _x77 _x78 ->
      case (_x76, _x77, _x78) of
          (n, f, s) -> ($) ((.) (view (currentTag s)) ((.) f (view n))) s