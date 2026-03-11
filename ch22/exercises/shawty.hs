{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty.Trans -- Note the use of .Trans

-- 1. Define a type synonym to make the signatures readable.
-- We are wrapping ReaderT (with a Redis Connection) over IO.
type Action = ActionT TL.Text (ReaderT R.Connection IO)
type Scotty = ScottyT TL.Text (ReaderT R.Connection IO)

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

-- Note: These helpers still take Connection because they are pure IO functions
saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

-- (UI helper functions linkShorty, shortyCreated, etc. remain the same)
linkShorty :: String -> String
linkShorty shorty = concat [ "<a href=\"", shorty, "\">Copy and paste your short URL</a>" ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty = TL.concat [ TL.pack (show resp), " shorty is: ", TL.pack (linkShorty shawty) ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri = TL.concat [ uri, " wasn't a url, did you forget http://?" ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

-- 2. The 'app' function no longer takes R.Connection as an argument.
-- It's now baked into the Scotty type.
app :: Scotty ()
app = do
  get "/" $ do
    -- 3. Use 'lift ask' to pull the connection out of the Reader environment
    rConn <- lift ask
    uri <- param "uri"
    let parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)

  get "/:short" $ do
    rConn <- lift ask
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  -- 4. scottyT requires a way to "run" your custom monad back into IO.
  -- (runReaderT ... rConn) handles the transformation for every request.
  scottyT 3000 (\m -> runReaderT m rConn) app