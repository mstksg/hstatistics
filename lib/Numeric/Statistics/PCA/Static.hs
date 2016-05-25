{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}

module Numeric.Statistics.PCA.Static (
                               pca, pcaN, pcaTransform, pcaReduce, pcaReduceN
                          ) where

-- import           Unsafe.Coerce
import           Data.Coerce
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy
import           Data.Type.Equality
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Data.Array                   as A
import qualified Numeric.LinearAlgebra        as LA
import qualified Numeric.Statistics.PCA       as S

pca :: forall m n r. (KnownNat m, KnownNat n)
    => L m n
    -> Double
    -> (forall j k. (KnownNat j, KnownNat k) => L j k -> r)
    -> r
pca d q f = withMatrix p $ \(p' :: L m' k) ->
              case sameNat (Proxy :: Proxy m) (Proxy :: Proxy m') of
                Nothing   -> error $ "static/dynamic mismatch "
                                  ++ show (natVal (Proxy :: Proxy m))
                                  ++ " "
                                  ++ show (natVal (Proxy :: Proxy m'))
                                  ++ " "
                                  ++ show (natVal (Proxy :: Proxy k))
                Just Refl -> f p'
  where
    m  = fromInteger $ natVal (Proxy :: Proxy m)
    -- I hope this fuses?
    d' = A.listArray (1, m) (extract <$> toRows d)
    p  = S.pca d' q

pcaN :: forall m n k. (KnownNat m, KnownNat n, KnownNat k)
     => L m n
     -> L m k
pcaN d = case create p of
           Nothing -> error "static/dynamic mismatch"
           Just p' -> p'
  where
    m  = fromInteger $ natVal (Proxy :: Proxy m)
    k  = fromInteger $ natVal (Proxy :: Proxy k)
    d' = A.listArray (1, m) (extract <$> toRows d)
    p  = S.pcaN d' k

pcaTransform :: forall m n k. (KnownNat m, KnownNat n, KnownNat k)
             => L m n
             -> L m k
             -> L k n
pcaTransform d m = underL (flip S.pcaTransform m') d
  where
    m' = extract m

pcaReduce :: forall m n. (KnownNat m, KnownNat n)
          => L m n
          -> Double
          -> L m n
pcaReduce d q = underL (flip S.pcaReduce q) d

pcaReduceN :: forall m n. (KnownNat m, KnownNat n)
           => L m n
           -> Int
           -> L m n
pcaReduceN d n = underL (flip S.pcaReduceN n) d

underL :: forall m n j k. (KnownNat m, KnownNat n, KnownNat j, KnownNat k)
       => (A.Array Int (LA.Vector Double) -> A.Array Int (LA.Vector Double))
       -> L m n
       -> L j k
underL f d = case create p of
               Nothing -> error "static/dynamic mismatch"
               Just p' -> p'
  where
    m  = fromInteger $ natVal (Proxy :: Proxy m)
    d' = A.listArray (1, m) (extract <$> toRows d)
    p  = LA.fromRows . A.elems $ f d'
