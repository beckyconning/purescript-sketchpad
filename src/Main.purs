module Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor (($>))

import Control.Coroutine
import Control.Coroutine.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff
import Network.HTTP.Affjax
import Control.Monad.Error.Class
import Control.Monad.Trans (lift)
import Data.Foreign

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Network.HTTP.ResponseHeader
import Network.HTTP.StatusCode

p :: Producer { status :: StatusCode, headers :: Prim.Array ResponseHeader, response :: Foreign } (Aff _) _
p = produce \emit -> launchAff do
  x <- attempt $ get "jashdflajsdfklajsfd"
  either (liftEff <<< emit <<< Right) (liftEff <<< emit <<< Left) x

c :: Consumer { status :: StatusCode, headers :: Prim.Array ResponseHeader, response :: Foreign } (Aff (console :: CONSOLE | _)) _
c = consumer \s -> liftEff (log $ show s.status) $> Nothing

main = runAff print print $ runProcess (p $$ c)
