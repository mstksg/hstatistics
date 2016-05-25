{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

import           Control.Monad.Random
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static hiding  ((===))
import           Numeric.Statistics.PCA.Static
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck
import qualified Data.Array                           as A
import qualified Numeric.LinearAlgebra                as LA
import qualified Numeric.Statistics.PCA               as S

-- pca_test :: Int -> Int -> Int

-- testUntyped :: IO ()
-- testUntyped = do
--     let 

main = defaultMain tests

tests = [ testProperty "pca" prop_pca ]

-- prop_pca :: Int -> Int ->
prop_pca n c mn mx q =
    (n > 10) ==>
    (c > 2) ==> 
    (mx > (mn + 1)) ==> 
    (q > 0) ==>
    case someNatVal n of
      Just (SomeNat (Proxy :: Proxy n)) ->
        case someNatVal c of
          Just (SomeNat (Proxy :: Proxy c)) ->
            let vs :: L n c
                vs = uniformSample 19034 (konst mn) (konst mx)
                p' = S.pca (rows vs) q
            in  pca vs q $ \res ->
                  extract res === p'

rows :: forall m n. (KnownNat n, KnownNat m)
     => L m n
     -> A.Array Int (LA.Vector Double)
rows = A.listArray (1, m) . fmap extract . toRows
  where
    m = fromInteger $ natVal (Proxy :: Proxy m)

-- rows' :: (KnownNat m, KnownNat n)
--       => A.Array Int (LA.Vector Double)
--       -> Maybe (L m n)
-- rows' = create . LA.fromRows . A.elems
