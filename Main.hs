{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (scotty, notFound, get, param, html, json, text)
import System.Environment (getEnvironment)
import Control.Monad (liftM)
import Data.Random (runRVar, randomElement, shuffleNofM)
import Data.Random.Source.DevRandom
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (object, toJSON)
import Data.Aeson.Types (ToJSON, (.=))
import Text.Blaze.Html5 (body, h1, p, toHtml, (!), a)
import Text.Blaze.Html5.Attributes (href)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Yaml (decodeFile)

data SpacecatCount = SpacecatCount
    { count :: Int
    } deriving Show

instance ToJSON SpacecatCount where
    toJSON (SpacecatCount count) = object ["spacecat_count" .= count]

data RandomCat = RandomCat
    { cat :: Text
    } deriving Show

instance ToJSON RandomCat where
    toJSON (RandomCat cat) = object ["spacecat" .= cat]

data CatBomb = CatBomb
    { cats :: [Text]
    } deriving Show

instance ToJSON CatBomb where
    toJSON (CatBomb cats) = object ["spacecats" .= cats]

randomCat :: [Text] -> IO Text
randomCat spacecats = do
    x <- runRVar (randomElement spacecats) DevRandom
    return $ x

catBomb :: [Text] -> Int -> IO [Text]
catBomb spacecats num = do
    let m = length spacecats
    let num' = minimum [num, m, 10]
    xs <- runRVar (shuffleNofM num' m spacecats) DevRandom
    return $ xs

readCats = do
    catInfo <- decodeFile "cats.yaml"
    return $ case catInfo of
                Nothing -> []
                Just cats -> cats

port = do
    env <- getEnvironment
    return $ HM.lookupDefault "3000" "PORT" (HM.fromList env)

main = do
    spacecats <- readCats
    port <- liftM read $ port
    scotty port $ do
        get "/" $ do
            html . renderHtml $ do
                body $ do
                    h1 "Random images for you"
                    a ! href "/random" $ "/random for a random image"
        get "/random" $ do
            cat <- liftIO $ randomCat spacecats
            json $ RandomCat {cat = cat}
        get "/count" $ do
            json $ SpacecatCount {count = length spacecats}
        get "/bomb" $ do
            num <- liftM read $ param "count"
            cats <- liftIO $ catBomb spacecats num
            json $ CatBomb {cats = cats}
        notFound $ do
            text "Page not found"
