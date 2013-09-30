{-# LANGUAGE PatternGuards #-}
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
        error, (.), (/=), (==), (<=), ($), (++), (>>=), (&&))
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import qualified Data.List as L
       (deleteBy, find, splitAt, filter, nub)
import Data.List ((\\))
import qualified Data.Map as M (Map, insert, delete, empty)
 
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
abort x = error $ "xmonad: StackSet: " ++ x
 
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m
  | not (null wids) && length m <= length wids && not (null m) =
    StackSet cur visi unseen M.empty
  where (seen, unseen)
          = L.splitAt (length m) $ map (\ i -> Workspace i l Nothing) wids
        (cur : visi)
          = do (i, s, sd) <- zip3 seen [0 ..] m
               return (Screen i s sd)
new _ _ _ = abort "non-positive argument to StackSet.new"
 
view ::
       (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
  | i == currentTag s = s
  | Just x <- L.find ((i ==) . tag . workspace) (visible s) =
    s{current = x,
      visible = current s : L.deleteBy (equating screen) x (visible s)}
  | Just x <- L.find ((i ==) . tag) (hidden s) =
    s{current = (current s){workspace = x},
      hidden =
        workspace (current s) : L.deleteBy (equating tag) x (hidden s)}
  | otherwise = s
  where equating f = \ x y -> f x == f y
 
greedyView ::
             (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView w ws
  | any wTag (hidden ws) = view w ws
  | (Just s) <- L.find (wTag . workspace) (visible ws) =
    ws{current = (current ws){workspace = workspace s},
       visible =
         s{workspace = workspace (current ws)} :
           L.filter (not . wTag . workspace) (visible ws)}
  | otherwise = ws
  where wTag = (w ==) . tag
 
lookupWorkspace :: (Eq s) => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w
  = listToMaybe
      (do Screen i s _ <- current w : visible w
          guard (s == sc)
          return (tag i))
 
with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current
 
modify ::
       Maybe (Stack a) ->
         (Stack a -> Maybe (Stack a)) ->
           StackSet i l a s sd -> StackSet i l a s sd
modify d f s
  = s{current =
        (current s){workspace =
                      (workspace (current s)){stack = with d f s}}}
 
modify' ::
        (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = modify Nothing (Just . f)
 
peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (return . focus)
 
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r
 
integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate
 
differentiate :: [a] -> Maybe (Stack a)
differentiate [] = Nothing
differentiate (x : xs) = Just $ Stack x [] xs
 
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs)
  = case L.filter p (f : rs) of
        f' : rs' -> Just $ Stack f' (L.filter p ls) rs'
        [] -> case L.filter p ls of
                  f' : ls' -> Just $ Stack f' ls' []
                  [] -> Nothing
 
index :: StackSet i l a s sd -> [a]
index = with [] integrate
 
focusUp, focusDown, swapUp, swapDown ::
         StackSet i l a s sd -> StackSet i l a s sd
focusUp = modify' focusUp'
focusDown = modify' focusDown'
swapUp = modify' swapUp'
swapDown = modify' (reverseStack . swapUp' . reverseStack)
 
focusUp', focusDown' :: Stack a -> Stack a
focusUp' (Stack t (l : ls) rs) = Stack l ls (t : rs)
focusUp' (Stack t [] rs) = Stack x xs []
  where (x : xs) = reverse (t : rs)
focusDown' = reverseStack . focusUp' . reverseStack
 
swapUp' :: Stack a -> Stack a
swapUp' (Stack t (l : ls) rs) = Stack t ls (l : rs)
swapUp' (Stack t [] rs) = Stack t (reverse rs) []
 
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls
 
focusWindow ::
              (Eq s, Eq a, Eq i) =>
              a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s
  | Just w == peek s = s
  | otherwise =
    fromMaybe s $
      do n <- findTag w s
         return $ until ((Just w ==) . peek) focusUp (view n s)
 
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s
 
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces s
  = workspace (current s) : map workspace (visible s) ++ hidden s
 
allWindows :: (Eq a) => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces
 
currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current
 
tagMember :: (Eq i) => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . map tag . workspaces
 
renameTag ::
            (Eq i) => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag o n = mapWorkspace rename
  where rename w = if tag w == o then w{tag = n} else w
 
ensureTags ::
             (Eq i) => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags l allt st = et allt (map tag (workspaces st) \\ allt) st
  where et [] _ s = s
        et (i : is) rn s | i `tagMember` s = et is rn s
        et (i : is) [] s
          = et is [] (s{hidden = Workspace i l Nothing : hidden s})
        et (i : is) (r : rs) s = et is rs $ renameTag r i s
 
mapWorkspace ::
             (Workspace i l a -> Workspace i l a) ->
               StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace f s
  = s{current = updScr (current s), visible = map updScr (visible s),
      hidden = map f (hidden s)}
  where updScr scr = scr{workspace = f (workspace scr)}
 
mapLayout ::
          (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout f (StackSet v vs hs m)
  = StackSet (fScreen v) (map fScreen vs) (map fWorkspace hs) m
  where fScreen (Screen ws s sd) = Screen (fWorkspace ws) s sd
        fWorkspace (Workspace t l s) = Workspace t (f l) s
 
member :: (Eq a) => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)
 
findTag :: (Eq a) => a -> StackSet i l a s sd -> Maybe i
findTag a s
  = listToMaybe
      (do w <- workspaces s
          guard (has a (stack w))
          return (tag w))
  where has _ Nothing = False
        has x (Just (Stack t l r)) = x `elem` (t : l ++ r)
 
insertUp ::
           (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s = if member a s then s else insert
  where insert
          = modify (Just $ Stack a [] [])
              (\ (Stack t l r) -> Just $ Stack a l (t : r))
              s
 
delete ::
         (Ord a, Eq s) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete w = sink w . delete' w
 
delete' ::
          (Eq a, Eq s) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w s
  = s{current = removeFromScreen (current s),
      visible = map removeFromScreen (visible s),
      hidden = map removeFromWorkspace (hidden s)}
  where removeFromWorkspace ws
          = ws{stack = stack ws >>= filter (/= w)}
        removeFromScreen scr
          = scr{workspace = removeFromWorkspace (workspace scr)}
 
float ::
        (Ord a) =>
        a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float w r s = s{floating = M.insert w r (floating s)}
 
sink :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
sink w s = s{floating = M.delete w (floating s)}
 
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster
  = modify' $
      \ c ->
        case c of
            Stack _ [] _ -> c
            Stack t ls rs -> Stack t [] (xs ++ x : rs)
              where (x : xs) = reverse ls
 
shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster
  = modify' $
      \ c ->
        case c of
            Stack _ [] _ -> c
            Stack t ls rs -> Stack t [] (reverse ls ++ rs)
 
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster
  = modify' $
      \ c ->
        case c of
            Stack _ [] _ -> c
            Stack t ls rs -> Stack x [] (xs ++ t : rs)
              where (x : xs) = reverse ls
 
shift ::
        (Ord a, Eq s, Eq i) =>
        i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\ w -> shiftWin n w s) (peek s)
 
shiftWin ::
           (Ord a, Eq a, Eq s, Eq i) =>
           i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s
  = case findTag w s of
        Just from | n `tagMember` s && n /= from -> go from s
        _ -> s
  where go from
          = onWorkspace n (insertUp w) . onWorkspace from (delete' w)
 
onWorkspace ::
              (Eq i, Eq s) =>
              i ->
                (StackSet i l a s sd -> StackSet i l a s sd) ->
                  (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s