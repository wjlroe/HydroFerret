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

spacecats = ["http://29.media.tumblr.com/tumblr_lxxabzSK0S1qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_lxxadsazU71qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_lxwnkmC49l1qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_lxwo23kO6U1qg4pufo1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_lxwo7yaRqK1qg4pufo1_500.jpg"
             , "http://28.media.tumblr.com/tumblr_lxwod6DYKY1qg4pufo1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_lxwouhvzGB1qg4pufo1_500.jpg"
             , "http://30.media.tumblr.com/tumblr_lxwqpqs9gB1qg4pufo1_500.jpg"
             , "http://27.media.tumblr.com/tumblr_lrxv4ycUTs1qg4pufo1_500.jpg"
             , "http://29.media.tumblr.com/tumblr_lq3hkjw1An1qg4pufo1_r1_500.jpg"
             , "http://26.media.tumblr.com/tumblr_lqcjwsc50Z1qg4pufo1_r2_500.jpg"
             , "http://25.media.tumblr.com/tumblr_lptsa5pVwJ1qg4pufo1_500.jpg"
             , "http://27.media.tumblr.com/tumblr_lpqiafbMzK1qg4pufo1_500.jpg"
             , "http://30.media.tumblr.com/tumblr_lifn4renxh1qg4pufo1_500.jpg"
             , "http://29.media.tumblr.com/tumblr_li6ahwTbUu1qg4pufo1_500.jpg"
             , "http://26.media.tumblr.com/tumblr_li69h8rl7k1qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_lgh4hdURXC1qg4pufo1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_lgdm8dR3mk1qg4pufo1_500.jpg"
             , "http://27.media.tumblr.com/tumblr_lg4fvkXwIP1qg4pufo1_500.jpg"
             , "http://29.media.tumblr.com/tumblr_lfr9wfkH381qg4pufo1_500.jpg"
             , "http://30.media.tumblr.com/tumblr_lfpjjmkm4Y1qg4pufo1_500.jpg"
             , "http://26.media.tumblr.com/tumblr_lfpgceLq6W1qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_lfjvgjWFsV1qg4pufo1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_lfju7nQIw01qg4pufo1_500.jpg"
             , "http://29.media.tumblr.com/tumblr_lf8t4bsxdc1qg4pufo1_500.jpg"
             , "http://30.media.tumblr.com/tumblr_lf6w1jWtUL1qg4pufo1_500.jpg"
             , "http://27.media.tumblr.com/tumblr_leocenmrlD1qg4pufo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_m658uupsqc1ryt75lo1_400.jpg"
             , "http://24.media.tumblr.com/tumblr_m61nq1TJV01qanywro1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_m5uqvhqCtA1qbhfooo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_m5rgi3NFf01qhygzco1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_m5l4hkapFO1r82738o1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_m46kpt4dL61rqojsso1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_m3wf50ptIS1qkacdoo1_500.gif"
             , "http://24.media.tumblr.com/tumblr_m376e363Tf1rpol69o1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_m2r8ub5pSS1qiv69zo1_500.jpg"
             , "http://24.media.tumblr.com/tumblr_m2dlrqkLgJ1rt5a6io1_500.jpg"
             , "http://25.media.tumblr.com/tumblr_m1vt2yG9hT1r3735bo1_500.jpg"
             ]

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

randomCat = do
    x <- runRVar (choice spacecats) DevRandom
    return $ x

catBomb num = do
    let num' = minimum [num, (length spacecats), 10]
    xs <- runRVar (choices num' spacecats) DevRandom
    return $ xs

port = do
    env <- getEnvironment
    return $ HM.lookupDefault "3000" "PORT" (HM.fromList env)

main = do
    port <- liftM read $ port
    scotty port $ do
        get "/" $ do
            html . renderHtml $ do
                body $ do
                    h1 "SPACECATSLOLOLOLOLOLOL!"
                    p "you like?"
        get "/random" $ do
            cat <- liftIO $ randomCat
            json $ RandomCat {cat = cat}
        get "/count" $ do
            json $ SpacecatCount {count = length spacecats}
        get "/bomb" $ do
            num <- liftM read $ param "count"
            cats <- liftIO $ catBomb num
            json $ CatBomb {cats = cats}
        notFound $ do
            text "Page not fount"
