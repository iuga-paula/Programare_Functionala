--import           Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity   :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity   :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Example 1 - Trivial
 
data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivId    = Trivial -> Bool
{- 
testTrivial :: IO ()
testTrivial
  = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: TrivId)
    quickCheck (monoidRightIdentity :: TrivId) -}

-- Exercise 2 - Identity
 
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity( x <> y )

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary 


type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
type IdentityId a = Identity a -> Bool


-- Exercise 3 - Pair
 
data Two a b = Two a b
  deriving (Eq, Show)

instance  (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary
  
  --EX 4 cu Three tema


  -- Exercise 5 - Boolean conjunction
 
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

 -- Exercise 5 - Boolean disjunction

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup  BoolDisj where
     BoolDisj a <>  BoolDisj b =  BoolDisj (a || b)

instance Monoid BoolConj where
    mempty = BoolDisj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

  --tema restul ;-((
      