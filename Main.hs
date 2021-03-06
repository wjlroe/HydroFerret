{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (scotty, notFound, get, matchAny, param, html, json, text)
import System.Environment (getEnvironment)
import Control.Monad (liftM)
import Data.Random (sample, randomElement, shuffleNofM)
import Data.Random.Source.IO()
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (object, toJSON)
import Data.Aeson.Types (ToJSON, (.=))
import Text.Blaze.Html5 (body, h1, (!), a)
import Text.Blaze.Html5.Attributes (href)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Yaml (decodeFile)

data SlackAttachment = SlackAttachment
    { title :: Text
    , fields :: [Text]
    , fallback :: Text
    , imageUrl :: Text
    } deriving Show

instance ToJSON SlackAttachment where
    toJSON (SlackAttachment titleField fieldsField fallbackField imageUrlField) = object
        [ "title" .= titleField
        , "fields" .= fieldsField
        , "fallback" .= fallbackField
        , "image_url" .= imageUrlField
        ]

data SlackMessage = SlackMessage
    { attachments :: [SlackAttachment]
    } deriving Show

instance ToJSON SlackMessage where
    toJSON (SlackMessage attachmentsField) = object
        [ "attachments" .= attachmentsField ]

data SpacecatCount = SpacecatCount
    { count :: Int
    } deriving Show

instance ToJSON SpacecatCount where
    toJSON (SpacecatCount spaceCatCount) = object ["spacecat_count" .= spaceCatCount]

data RandomCat = RandomCat
    { cat :: Text
    } deriving Show

instance ToJSON RandomCat where
    toJSON (RandomCat catUrl) = object ["spacecat" .= catUrl]

data CatBomb = CatBomb
    { cats :: [Text]
    } deriving Show

instance ToJSON CatBomb where
    toJSON (CatBomb catList) = object ["spacecats" .= catList]

newAttachment :: Text -> SlackAttachment
newAttachment imageUrlI =
    SlackAttachment { title = "Spacecat"
                    , fallback = "A spacecat"
                    , fields = []
                    , imageUrl = imageUrlI
                    }

newSlackMessage :: Text -> SlackMessage
newSlackMessage imageUrlI =
    SlackMessage { attachments = [newAttachment imageUrlI] }

randomCat :: [Text] -> IO Text
randomCat spacecats = do
    x <- sample (randomElement spacecats)
    return $ x

catBomb :: [Text] -> Int -> IO [Text]
catBomb spacecats num = do
    let m = length spacecats
    let num' = minimum [num, m, 10]
    xs <- sample (shuffleNofM num' m spacecats)
    return $ xs

readCats :: IO [Text]
readCats = do
    catInfo <- decodeFile "cats.yaml"
    return $ case catInfo of
                Nothing -> []
                Just catList -> catList

portConfig :: IO String
portConfig = do
    env <- getEnvironment
    return $ HM.lookupDefault "3000" "PORT" (HM.fromList env)

main :: IO ()
main = do
    spacecats <- readCats
    port <- liftM read $ portConfig
    scotty port $ do
        get "/" $ do
            html . renderHtml $ do
                body $ do
                    h1 "Random images for you"
                    a ! href "/random" $ "/random for a random image"
        matchAny "/random" $ do
            randcat <- liftIO $ randomCat spacecats
            json $ RandomCat {cat = randcat}
        matchAny "/slack/random" $ do
            randcat <- liftIO $ randomCat spacecats
            json $ newSlackMessage randcat
        get "/count" $ do
            json $ SpacecatCount {count = length spacecats}
        get "/bomb" $ do
            num <- liftM read $ param "count"
            catList <- liftIO $ catBomb spacecats num
            json $ CatBomb {cats = catList}
        notFound $ do
            text "Page not found"
