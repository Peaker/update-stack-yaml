{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString as BS
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Yaml
import           Data.Yaml.Pretty
import           System.FilePath
import           System.Process

parseLine :: T.Text -> Maybe (T.Text, T.Text)
parseLine line = case T.break (=='=') line of
    (name, uncons -> Just ('=', val)) -> Just (name, val)
    _ -> Nothing

refOfRepo :: T.Text -> IO T.Text
refOfRepo "" = return ""
refOfRepo repoPath =
    T.stripEnd . T.pack
    <$> readProcess "git" ["--git-dir", T.unpack repoPath, "rev-parse", "HEAD"] ""

type GitHash = T.Text

editLocation :: [(T.Text, GitHash)] -> Value -> Value
editLocation hashes loc =
    case lookup name hashes of
    Nothing -> error $ "Missing: " ++ show name
    Just "" -> loc -- ignore
    Just newHash ->
        loc & key "commit" .~ _String # newHash
    where
        Just gitRepo = loc ^? key "git" . _String
        name = T.pack $ takeBaseName $ T.unpack gitRepo

main :: IO ()
main = do
    hashes <-
        T.getContents
        <&> T.lines
        <&> mapM parseLine
        <&> fromMaybe (error "Failed to parse stdin")
        >>= traversed . _2 %%~ refOfRepo
    Just stack <- decodeFile "stack.yaml" :: IO (Maybe Value)
    stack
        & key "packages" . values . key "location" %~ editLocation hashes
        & encodePretty defConfig
        & BS.writeFile "stack.yaml"
