{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Yaml (FromJSON, Parser, ToJSON (..), Value (String), decodeFileThrow, encodeFile)
import Data.Yaml.Aeson (FromJSON (..), withText)
import GHC.Generics (Generic)
import Main.Utf8 (withUtf8)
import System.Directory.Extra (createDirectoryIfMissing, removeFile)
import System.FilePath.Posix
import Text.Read (readMaybe)

main :: IO ()
main = withUtf8 do
  let testDir = "."
  config <- decodeFileThrow @_ @TestConfig (testDir </> "config.yaml")
  forM_ (filter (fromMaybe True . (.enable)) config.sets) $ \set -> do
    test@Test{source, meta} <- parseTest set.source
    let exclude = fromMaybe [] set.exclude
        include = fromMaybe (test.programs <&> (.name)) set.include & filter (`notElem` exclude)
        programs = filter (\x -> x.name `elem` include) test.programs
        testContent = TestContent{..}

    -- write yaml
    let target = set.yaml
        targetTmp = target <.> ".tmp"
    createDirectoryIfMissing True (takeDirectory target)
    encodeFile targetTmp testContent
    readFile targetTmp >>= appendFile target
    removeFile targetTmp

    -- write eo
    createDirectoryIfMissing True (takeDirectory set.destination)
    writeFile set.destination meta
    forM_ programs (\x -> appendFile set.destination x.text)

data TestSet = TestSet
  { source :: FilePath
  , yaml :: FilePath
  , destination :: FilePath
  , include :: Maybe [String]
  -- ^
  -- Program names to include.
  --
  -- `Nothing` is equivalent to all programs.
  , exclude :: Maybe [String]
  -- ^
  -- Program names to exclude
  --
  -- `Nothing` is equivalent to no programs.
  , enable :: Maybe Bool
  -- ^
  -- Whether to enable this test set.
  }
  deriving (Show, Generic, FromJSON)

data TestConfig = TestConfig
  { sets :: [TestSet]
  }
  deriving (Show, Generic, FromJSON)

data Pos = Pos
  { file :: FilePath
  , line :: Int
  }
  deriving (Show)

instance ToJSON Pos where
  toJSON :: Pos -> Value
  toJSON Pos{..} = String (fromString (file <> ":" <> show line))

instance FromJSON Pos where
  parseJSON :: Value -> Parser Pos
  parseJSON = withText "Pos" $ \(T.unpack -> x) -> do
    let (file, rs) = span (/= ':') x
    guard (not . null $ file)
    guard (length rs > 1)
    let line' = readMaybe (drop 1 rs)
    maybe (fail $ x <> " is not a number") (\line -> pure Pos{..}) line'

data Program = Program
  { source :: Pos
  , name :: String
  , text :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Test = Test
  { source :: String
  , license :: String
  , meta :: String
  , programs :: [Program]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data TestContent = TestContent
  { source :: FilePath
  , meta :: String
  , programs :: [Program]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

parseProgramsRaw :: ([(Int, [String])], (Int, [[Char]]), Int) -> [[Char]] -> [(Int, String)]
parseProgramsRaw (programs', (programStart, program), curLine) (line'@(x : _) : xs)
  | (program /= [] && head program == "" || null program) && (x == '[' || x == '#') = parseProgramsRaw ((programStart, program) : programs', (curLine, [line']), curLine + 1) xs
  | otherwise = parseProgramsRaw (programs', (programStart, line' : program), curLine + 1) xs
parseProgramsRaw (programs', (programStart, program), curLine) ("" : xs) = parseProgramsRaw (programs', (programStart, "" : program), curLine + 1) xs
parseProgramsRaw (programs', program, _) [] = (unlines <$>) <$> drop 1 (reverse ((reverse <$>) <$> (program : programs')))

parseTest' :: FilePath -> [String] -> Test
parseTest' source eoCode =
  let
    (license, k') = span (\case '#' : _ -> True; "" -> True; _ -> False) eoCode
    (meta, k'') = span (\case '+' : _ -> True; "" -> True; _ -> False) k'
    programsStart = length license + length meta + 1
    programsRaw = parseProgramsRaw ([], (programsStart, []), programsStart) k''
    programs = programsRaw <&> (\(line, text) -> Program{source = Pos{file = source, ..}, name = text & dropWhile (/= '[') & drop 5 & takeWhile (/= '\n'), ..})
   in
    Test{license = unlines license, meta = unlines meta, ..}

parseTest :: FilePath -> IO Test
parseTest path = readFile path <&> (parseTest' path . lines)
