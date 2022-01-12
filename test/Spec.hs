{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, bracket, catch, mask_, throw)
import Control.Monad (forM_, unless)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import DLPack
import Data.IORef
import Data.Int (Int64)
import qualified Data.Matrix.MatrixMarket as Market
import Data.Proxy
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable, eqT)
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Debug.Trace
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar, CInt (..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable
import qualified Foreign.Storable as Storable

data DenseMatrix a = DenseMatrix {dmData :: !(Vector a), dmShape :: !(Int, Int)}

instance (Storable a, IsDLDataType a) => IsDLTensor IO (DenseMatrix a) where
  withDLTensor (DenseMatrix v (r, c)) action =
    S.unsafeWith v $ \p -> viaContiguousBuffer p [r, c] [c, 1] action

fromSparseMatrix :: (Storable a, RealFloat a) => Market.Matrix Scientific -> DenseMatrix a
fromSparseMatrix m = case m of
  Market.RMatrix (r, c) _ _ coords ->
    runST $ do
      v <- SM.new (r * c)
      forM_ coords $ \(i, j, x) ->
        SM.write v ((i - 1) * c + (j - 1)) (Scientific.toRealFloat x)
      DenseMatrix <$> S.unsafeFreeze v <*> pure (r, c)
  _ -> error "unsupported format"

fromDenseVector :: (Storable a, RealFloat a) => Market.Array Scientific -> Vector a
fromDenseVector m = case m of
  Market.RArray (r, c@1) _ elements ->
    S.fromListN (r * c) (Scientific.toRealFloat <$> elements)
  _ -> error "unsupported format"

loadMatrix :: (Storable a, RealFloat a) => FilePath -> IO (DenseMatrix a)
loadMatrix path = fromSparseMatrix <$> Market.readMatrix path

loadVector :: (Storable a, RealFloat a) => FilePath -> IO (Vector a)
loadVector path = fromDenseVector <$> Market.readArray path

type BlasInt = CInt

type Cgemv a =
  -- | trans
  Ptr CChar ->
  -- | m
  Ptr BlasInt ->
  -- | n
  Ptr BlasInt ->
  -- | alpha
  Ptr a ->
  -- | a
  Ptr a ->
  -- | lda
  Ptr BlasInt ->
  -- | x
  Ptr a ->
  -- | incx
  Ptr BlasInt ->
  -- | beta
  Ptr a ->
  -- | y
  Ptr a ->
  -- | incy
  Ptr BlasInt ->
  IO ()

foreign import ccall unsafe "sgemv_" sgemv :: Cgemv Float

foreign import ccall unsafe "dgemv_" dgemv :: Cgemv Double

machineEpsilon :: forall a. RealFloat a => a
machineEpsilon = encodeFloat 1 (-(floatDigits (1 :: a) - 1))

allclose :: forall a. (Storable a, RealFloat a, Show a) => Vector a -> Vector a -> Bool
allclose x y =
  trace ("rtol = " <> show rtol <> ", atol = " <> show atol) $
    S.all id $ S.zipWith check x y
  where
    !rtol = sqrt machineEpsilon
    !atol = 10 * machineEpsilon
    check !a !b = abs (a - b) < rtol * (max (abs a) (abs b)) + atol

matvec ::
  forall a.
  (Typeable a, Storable a, Num a) =>
  DenseMatrix a ->
  Vector a ->
  MVector (PrimState IO) a ->
  IO ()
matvec m x y
  | dmShape m == (SM.length y, S.length x) =
    S.unsafeWith (dmData m) $ \aPtr ->
      S.unsafeWith x $ \xPtr ->
        SM.unsafeWith y $ \yPtr ->
          with (castCharToCChar 'T') $ \transPtr ->
            with (fromIntegral $ SM.length y) $ \mPtr ->
              with (fromIntegral $ S.length x) $ \nPtr ->
                with (1 :: CInt) $ \onePtr ->
                  with (1 :: a) $ \alphaPtr ->
                    with (0 :: a) $ \betaPtr ->
                      c_gemv transPtr mPtr nPtr alphaPtr aPtr nPtr xPtr onePtr betaPtr yPtr onePtr
  | otherwise = error "shapes do not match"
  where
    c_gemv :: Cgemv a
    c_gemv
      | Just Refl <- eqT @a @Float = sgemv
      | Just Refl <- eqT @a @Double = dgemv
      | otherwise = error "unsupported type"

viewAsVector :: forall a. (Storable a, IsDLDataType a) => DLTensor -> IO (MVector (PrimState IO) a)
viewAsVector t = case dlAsContiguousVector t of
  Left e -> error (T.unpack e)
  Right (ContiguousVectorView size ptr) -> SM.MVector size <$> newForeignPtr_ ptr

asLinearOperator ::
  (Show a, Typeable a, Storable a, Num a, IsDLDataType a) =>
  DenseMatrix a ->
  DLTensor ->
  DLTensor ->
  IO ()
asLinearOperator m = apply
  where
    apply !src !dst = do
      x <- S.unsafeFreeze =<< viewAsVector src
      y <- viewAsVector dst
      matvec m x y

type LinearOperator = Ptr DLTensor -> Ptr DLTensor -> IO CInt

foreign import ccall "wrapper"
  mkLinearOperator :: LinearOperator -> IO (FunPtr LinearOperator)

withLinearOperator ::
  (DLTensor -> DLTensor -> IO ()) ->
  (FunPtr LinearOperator -> IO b) ->
  IO b
withLinearOperator f action = do
  ref <- newIORef Nothing
  let f' !srcPtr !dstPtr = do
        !src <- peek srcPtr
        !dst <- peek dstPtr
        () <- f src dst
        pure 0
      f'' srcPtr dstPtr =
        catch
          (f' srcPtr dstPtr)
          (\(e :: SomeException) -> writeIORef ref (Just e) >> pure (-1))
  r <- bracket (mkLinearOperator f'') freeHaskellFunPtr action
  readIORef ref >>= \e -> case e of
    Just e -> throw e
    Nothing -> pure r

data {-# CTYPE "bicgstab_hs_parameters" #-} RawOptions = RawOptions
  { rawOptionsDimension :: !Int64,
    rawOptionsMatrix :: !(FunPtr LinearOperator),
    rawOptionsMaxIters :: !Int64,
    rawOptionsTol :: !Double,
    rawOptionsB :: !(Ptr DLTensor),
    rawOptionsX0 :: !(Ptr DLTensor),
    rawOptionsX :: !(Ptr DLTensor)
  }

instance Storable RawOptions where
  sizeOf _ = 56
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  peek p =
    RawOptions
      <$> peekByteOff p 0
      <*> peekByteOff p 8
      <*> peekByteOff p 16
      <*> peekByteOff p 24
      <*> peekByteOff p 32
      <*> peekByteOff p 40
      <*> peekByteOff p 48
  {-# INLINE peek #-}
  poke p o = do
    pokeByteOff p 0 (rawOptionsDimension o)
    pokeByteOff p 8 (rawOptionsMatrix o)
    pokeByteOff p 16 (rawOptionsMaxIters o)
    pokeByteOff p 24 (rawOptionsTol o)
    pokeByteOff p 32 (rawOptionsB o)
    pokeByteOff p 40 (rawOptionsX0 o)
    pokeByteOff p 48 (rawOptionsX o)
  {-# INLINE poke #-}

foreign import ccall safe "bicgstab.h bicgstab_hs_solve"
  bicgstab_hs_solve :: Ptr RawOptions -> IO CInt

data BiCGStabOptions = BiCGStabOptions
  { bicgstabDim :: !Int,
    bicgstabMaxIters :: !Int,
    bicgstabTol :: !Double
  }

withDLTensorPtr :: IsDLTensor IO a => a -> (Ptr DLTensor -> IO b) -> IO b
withDLTensorPtr a action =
  withDLTensor a $ \t ->
    with t $ \tPtr ->
      action tPtr

bicgstab ::
  (IsDLTensor IO v, IsDLTensor IO mv) =>
  BiCGStabOptions ->
  (DLTensor -> DLTensor -> IO ()) ->
  v ->
  Maybe v ->
  mv ->
  IO ()
bicgstab opts mv b x₀ x =
  withLinearOperator mv $ \matrixPtr ->
    withDLTensorPtr b $ \bPtr ->
      withDLTensorPtr x $ \xPtr -> do
        let rawOpts =
              RawOptions
                (fromIntegral (bicgstabDim opts))
                matrixPtr
                (fromIntegral (bicgstabMaxIters opts))
                (bicgstabTol opts)
                bPtr
                nullPtr
                xPtr
        i <- with rawOpts $ bicgstab_hs_solve
        print i

instance (Storable a, IsDLDataType a) => IsDLTensor IO (Vector a) where
  withDLTensor v action = S.unsafeWith v $ \p ->
    viaContiguousBuffer p [S.length v] [1] action

instance (Storable a, IsDLDataType a, s ~ PrimState IO) => IsDLTensor IO (MVector s a) where
  withDLTensor v action = SM.unsafeWith v $ \p ->
    viaContiguousBuffer p [SM.length v] [1] action

main :: IO ()
main = do
  m1 <- loadMatrix @Double "test/m1.mtx"
  b1 <- loadVector @Double "test/b1.mtx"
  x1 <- loadVector @Double "test/x1.mtx"
  t <- SM.new (S.length b1)
  matvec m1 b1 t
  t' <- S.unsafeFreeze t
  print $ allclose t' (S.fromList [0.7161821090127248, 0.2262401030402707])

  x <- SM.new (S.length b1) :: IO (MVector (PrimState IO) Double)
  bicgstab
    (BiCGStabOptions (S.length b1) (S.length b1) 1.0e-5)
    (asLinearOperator m1)
    b1
    Nothing
    x
  x' <- S.unsafeFreeze x
  print $ allclose x' x1

  putStrLn ("Test suite is not implemented" :: String)
