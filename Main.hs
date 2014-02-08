{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (scotty, notFound, get, param, html, json, text)
import System.Environment (getEnvironment)
import Control.Monad (liftM)
import Data.Random.Extras (choice, choices)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (object, toJSON)
import Data.Aeson.Types (ToJSON, (.=))
import Text.Blaze.Html5 (body, h1, p)
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

randomCat spacecats = do
    x <- runRVar (choice spacecats) DevRandom
    return $ x

catBomb spacecats num = do
    let num' = minimum [num, (length spacecats), 10]
    xs <- runRVar (choices num' spacecats) DevRandom
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
                    h1 "SPACECATSLOLOLOLOLOLOL!"
                    p "you like?"
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
            text "Page not fount"
