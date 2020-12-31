import           Prelude hiding (lookup)
import qualified Data.List as List

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value

  keys :: c key value -> [key]
  keys = map fst . toList


  values :: c key value -> [value]
  values = map snd. toList


  toList :: c key value -> [(key, value)]


  fromList :: Ord key => [(key,value)] -> c key value
  fromList = foldr (uncurry insert) empty


--1 mai sus
--uncurry (a0 -> b0 -> c0) -> (a0, b0) -> c0)
  


newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where --pt o instanta e nevoie de implementarea metodelor din clasa (care e ca o interfata => poate contine sau nu definitii implicite)
    --empty :: PairList key value
    empty = PairList []
    --singleton :: key -> value -> PairList key value
    singleton  key value = PairList[(key,value)]
    --insert
     -- :: Ord key
     -- => key -> value -> c key value -> c key value
    insert key value  coll  = PairList((key, value) : getPairList coll)
    --lookup :: Ord key => key -> c key value -> Maybe value
    lookup k coll = getFirst( filter(\(key,val) -> key == k)(getPairList coll)) --iau doar elementele care au cheia data si din astea iau primul elem
        where
            getFirst :: [(k,v)] -> Maybe v
            getFirst [] =  Nothing
            getFirst (x:_) = Just(snd x)
    --delete :: Ord key => key -> c key value -> c key value
    delete k coll = PairList(filter(\(key, val) -> key /= k)(getPairList coll))
    toList coll = getPairList coll


--2

data SearchTree key value
  = Empty
  | Node
      (SearchTree key value) -- elemente cu cheia mai mica 
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

order :: Integer
order = 1


instance Collection SearchTree where
    lookup _ Empty = Nothing
    lookup key (Node leftThree currentKey currentValue rightTree)
       | key < currentKey =
           lookup key leftThree
       | key > currentKey = 
           lookup key rightTree
        | otherwise = currentValue

--3 
data Element k v
  = Element k (Maybe v)
  | OverLimit
--a
instance Eq k => Eq(Element k v) where
    (Element k1 v1) == (Element k2 v2) = k1 == k1
    --(Element k1 v1) /= (Element k2 v2) = k2 /= k1 --face el /=
    (Element _ _) == OverLimit = False 
    OverLimit == (Element _ _) = False
    OverLimit == OverLimit = True


--b
instance Ord k => Ord (Element k v) where
    (Element k1 _) <= (Element _ _) = k1 <= k1
    OverLimit <= (Element _ _) = False
    (Element _ _) <= OverLimit = True

--c
instance (Show k, Show v) => Show (Element k v) where
    show (Element k (Just v)) = "("++show k ++ "," ++ show v ++ ")"
    show (Element k Nothing) = show k
    show OverLimit = ""


--4
data BTree key value
  = BEmpty
  | BNode [(BTree key value, Element key value)]


instance Collection BTree where
      empty  = BEmpty
      singleton a b = BNode[(BEmpty, Element a (Just b)), (BEmpty, OverLimit)]
      lookup _ BEmpty =  Nothing
      lookup k (BNode list) = go list 
        where
            go ((less_than_key, OverLimit) : list') = lookup k less_than_key
            go ((less_than_key, Element k' v') : list')
                | k == k' = v'
                | k' < k = lookup k less_than_key  --cheia cautata e mai mare decat cheia curenta 
                | k' > k = go list'

      delete _ BEmpty =  BEmpty
      delete k (BNode list) = BNode(go list) 
        where
            go ((less_than_key, OverLimit) : list') = (delete k less_than_key, OverLimit) : list'
            go ((less_than_key, Element k' v') : list')
                | k == k' = (less_than_key, Element k' Nothing) : list'
                | k' < k = ((delete k less_than_key, Element k' v') : list') --cheia cautata e mai mare decat cheia curenta 
                | k' > k = (less_than_key, OverLimit): go list'

      toList BEmpty = []
      toList (BNode list) = go list 
           where
               go ((less_than_key, OverLimit) : _) = toList less_than_key
               go ((less_than_key, Element key Nothing): list') = toList less_than_key ++ go list'
               go ((less_than_key, Element key (Just v)): list') = toList less_than_key ++ (key,v) : go list'

