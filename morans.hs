{-# LANGUAGE ViewPatterns #-}

-- Requires files from http://yann.lecun.com/exdb/mnist/
import Codec.Compression.GZip (decompress)
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (for_)
import Data.List
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Int (Int64)
import System.Environment
import System.Exit
import System.IO
import System.Random

type Biases = Vector Double

type Weights = Vector (Vector Double)

type NeuralNet = Vector (Biases, Weights)

gauss :: Double -> IO Double
gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

newBrain :: Vector Int -> IO NeuralNet
newBrain szs@(V.uncons -> Just (_, ts)) =
  V.zip (flip V.replicate 1 <$> ts)
    <$> V.zipWithM (\m n -> V.replicateM n $ V.replicateM m $ gauss 0.01) szs ts

-- activation function
relu :: Double -> Double
relu = max 0

-- derivative of activation function
relu' :: (Ord a, Num a, Num p) => a -> p
relu' x
  | x < 0 = 0
  | otherwise = 1

zLayer :: Vector Double -> (Vector Double, Vector (Vector Double)) -> Vector Double
zLayer as (bs, wvs) = V.zipWith (+) bs $ V.sum . V.zipWith (*) as <$> wvs

feed :: Vector Double -> NeuralNet -> Vector Double
feed = foldl' (((relu <$>) .) . zLayer)

-- xs: vector of inputs
-- Returns a list of (weighted inputs, activations) of each layer,
-- from last layer to first.
revaz :: Vector Double -> Vector (Vector Double, Vector (Vector Double)) -> (Vector (Vector Double), Vector (Vector Double))
revaz xs = V.foldl' f (V.singleton xs, V.empty)
  where
    f (avs, zs) (bs, wms) = (V.cons (relu <$> zs') avs, V.cons zs' zs)
      where
        zs' = zLayer av (bs, wms)
        av = V.head avs

dCost :: (Num p, Ord p) => p -> p -> p
dCost a y
  | y == 1 && a >= y = 0
  | otherwise = a - y

vtranspose :: Vector (Vector a) -> Vector (Vector a)
vtranspose (V.uncons -> Nothing) = V.empty
vtranspose (V.uncons -> Just (V.uncons -> Nothing, xss)) = vtranspose xss
vtranspose v = ((V.fromList <$>) . V.fromList) . transpose . (V.toList <$>) . V.toList $ v

-- xv: vector of inputs
-- yv: vector of desired outputs
-- Returns list of (activations, deltas) of each layer in order.
deltas :: Vector Double -> Vector Double -> NeuralNet -> (Vector (Vector Double), Vector (Vector Double))
deltas xv yv vlayers = (V.reverse avs, f (vtranspose . snd <$> V.reverse vlayers) zvs (V.singleton delta0))
  where
    (avs@(V.uncons -> Just (av, _)), V.uncons -> Just (zv, zvs)) = revaz xv vlayers
    delta0 = V.zipWith (*) (V.zipWith dCost av yv) (relu' <$> zv)
    f _ (V.uncons -> Nothing) dvs = dvs
    f (V.uncons -> Just (wm, wms)) (V.uncons -> Just (zv, zvs)) dvs@(V.uncons -> Just (dv, _)) =
      f wms zvs $ (`V.cons` dvs) $ V.zipWith (*) ((\row -> V.sum $ V.zipWith (*) row dv) <$> wm) (relu' <$> zv)

eta :: Double
eta = 0.002

descend :: Vector Double -> Vector Double -> Vector Double
descend av dv = V.zipWith (-) av ((eta *) <$> dv)

learn :: Vector Double -> Vector Double -> NeuralNet -> NeuralNet
learn xv yv layers = V.zip (V.zipWith descend aa dvs) (V.zipWith3 f bb avs dvs)
  where
    (aa, bb) = V.unzip layers
    f wvs av dv = V.zipWith (\wv d -> descend wv ((d *) <$> av)) wvs dv
    (avs, dvs) = deltas xv yv layers

getImage :: Num b => BS.ByteString -> Int64 -> Vector b
getImage s n =
  fromIntegral . BS.index s . (n * 28 * 28 + 16 +) <$> V.fromList [0 .. 28 * 28 - 1]

-- getX :: Fractional b => BS.ByteString -> Int64 -> [b]
getX s n = (/ 256) <$> getImage s n

getLabel :: Num b => BS.ByteString -> Int64 -> b
getLabel s n = fromIntegral $ BS.index s (n + 8)

-- getY :: Num b => BS.ByteString -> Int64 -> [b]
getY s n = V.fromList (fromIntegral . fromEnum . (getLabel s n ==) <$> [0 .. 9])

render :: Integral a => a -> Char
render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

main :: IO ()
main = do
  as <- getArgs
  [trainI, trainL, testI, testL] <-
    mapM
      ((decompress <$>) . BS.readFile)
      [ "train-images-idx3-ubyte.gz" :: FilePath,
        "train-labels-idx1-ubyte.gz",
        "t10k-images-idx3-ubyte.gz",
        "t10k-labels-idx1-ubyte.gz"
      ]

  when (as == ["samplesjs"]) $ do
    putStr $
      unlines
        [ "var samples = " ++ show (show . getImage testI <$> V.fromList [0 .. 49]) ++ ";",
          "function random_sample() {",
          "  return samples[Math.floor(Math.random() * samples.length)];",
          "}"
        ]
    exitSuccess

  hSetBuffering stderr LineBuffering
  let (pStr, pStrLn) = case as of
        ["print"] -> (hPutStr stderr, hPutStrLn stderr)
        _ -> (putStr, putStrLn)

  n <- (`mod` 10000) <$> randomIO
  pStr . unlines $ take 28 $ take 28 <$> iterate (drop 28) (V.toList (render <$> getImage testI n))

  b <- newBrain (V.fromList [784, 30, 10])
  let example = getX testI n
      bs =
        V.scanl'
          (V.foldl' (\b n -> learn (getX trainI n) (getY trainL n) b))
          b
          (V.fromList (V.fromList <$> [[0 .. 999], [1000 .. 2999], [3000 .. 5999], [6000 .. 9999]]))
      smart = V.last bs
      cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
      bestOf = fst . maximumBy (comparing snd) . zip [0 ..]

  for_ bs $ pStrLn . unlines . zipWith cute [0 .. 9] . V.toList . feed example

  pStrLn $ "best guess: " ++ show (bestOf $ V.toList (feed example smart))

  let guesses = bestOf . V.toList . (\n -> feed (getX testI n) smart) <$> V.fromList [0 .. 9999]
  let answers = getLabel testL <$> V.fromList [0 .. 9999]
  pStrLn $ show (sum $ fromEnum <$> V.zipWith (==) guesses answers) ++ " / 10000"

  case as of
    ["print"] -> print smart
    _ -> return ()
