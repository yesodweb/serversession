module Web.ServerSession.Frontend.Yesod.Internal
  (
  ) where


-- TODO: I'm in a bad shape :(.


import Data.Default (def)
import Web.Cookie (parseCookies, SetCookie(..))
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (setSessionBS)
import Yesod.Core.Types (Header(AddCookie), SaveSession, SessionBackend(..), SessionMap)
import qualified Network.Wai as W


-- | Construct the server-side session backend from the given state.
backend :: Storage s => State s -> SessionBackend
backend state =
  SessionBackend {
    sbLoadSession = loadSession state "JSESSIONID" -- LOL :)
  }



-- | Create a cookie for the given session ID.
createCookie :: ByteString -> SessionId -> Header
createCookie cookieName key =
  -- Generate a cookie with the final session ID.
  AddCookie def
    { setCookieName     = cookieName
    , setCookieValue    = TE.encodeUtf8 $ unS key
    , setCookiePath     = Just "/"
    , setCookieExpires  = Just undefined
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = True
    }


-- | Fetch the 'SessionId' from the cookie with the given name.
-- Returns @Nothing@ if:
--
--   * There are zero cookies with the given name.
--
--   * There is more than one cookie with the given name.
--
--   * The cookie's value isn't considered a 'SessionId'.  We're
--   a bit strict here.
findSessionId :: ByteString -> W.Request -> Maybe SessionId
findSessionId cookieName req = do
  let matching = do
        ("Cookie", header) <- W.requestHeaders req
        (k, v) <- parseCookies header
        guard (k == cookieName)
        return v
  [raw] <- return matching
  fromPathPiece (TE.decodeUtf8 raw)


-- | The session key used by @yesod-auth@ without depending on it.
authKey :: Text
authKey = "_ID"




-- | Invalidate the current session ID (and possibly more, check
-- 'ForceInvalidate').  This is useful to avoid session fixation
-- attacks (cf. <http://www.acrossecurity.com/papers/session_fixation.pdf>).
--
-- Note that the invalidate /does not/ occur when the call to
-- this action is made!  The sessions will be invalidated on the
-- end of the handler processing.  This means that later calls to
-- 'forceInvalidate' on the same handler will override earlier
-- calls.
forceInvalidate :: MonadHandler m => ForceInvalidate -> m ()
forceInvalidate = setSessionBS forceInvalidateKey . B8.pack . show
