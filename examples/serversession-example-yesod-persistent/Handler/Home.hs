-- | On this serversession example, we simply provide some ways
-- users may interact with the session.
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3

import qualified Data.Map as M
import qualified Web.ServerSession.Frontend.Yesod as SS


-- | Homepage.
getHomeR :: Handler Html
getHomeR = do
  (forceFormWidget,      forceFormEnctype)      <- generateFormPost forceForm
  (sessionAddFormWidget, sessionAddFormEnctype) <- generateFormPost sessionAddForm
  msid <- getSessionId
  vars <- M.toAscList <$> getSession
  defaultLayout $ do
    setTitle "Server-side session example"
    $(widgetFile "homepage")


-- | Invalidate the session as requested via 'forceForm'.
postForceR :: Handler ()
postForceR =
  processForm "Force form" forceForm $ \force -> do
    msid <- getSessionId
    SS.forceInvalidate force
    return $ concat
      [ "Forced session invalidation using "
      , show force
      , " [old session ID was "
      , show msid
      , "]." ]


-- | Add (or modify) a session variable.
postSessionAddR :: Handler ()
postSessionAddR =
  processForm "Add session form" sessionAddForm $ \(key, val) -> do
    setSession key val
    return $ concat
      [ "Set session key "
      , show key
      , " to value "
      , show val
      , "." ]


-- | Delete a session variable.
postSessionDeleteR :: Text -> Handler ()
postSessionDeleteR key = do
  deleteSession key
  setMessage $ toHtml $ "Deleted session key " ++ show key ++ "."
  redirect HomeR


----------------------------------------------------------------------


-- | Helper function for form processing handlers.
processForm :: String -> Form a -> (a -> Handler String) -> Handler ()
processForm formName form act = do
  ((result, _), _) <- runFormPost form
  (>>= setMessage . toHtml) $
    case result of
      FormSuccess ret  -> act ret
      FormFailure errs -> return $ formName ++ " has errors: " ++ show errs ++ "."
      FormMissing      -> return $ formName ++ " is missing."
  redirect HomeR


-- | Form for session invalidation.
forceForm :: Form SS.ForceInvalidate
forceForm =
  identifyForm "forceForm" $
  renderBootstrap3 horizontal $
       areq (selectField optionsEnum) "Kind of invalidation" (Just SS.DoNotForceInvalidate)
    <* submit "Force session invalidation!"


-- | Form for adding or modifying session variables.
sessionAddForm :: Form (Text, Text)
sessionAddForm =
  identifyForm "sessionAddForm" $
  renderBootstrap3 horizontal $
    (,)
      <$> areq textField "Session key"   Nothing
      <*> areq textField "Session value" Nothing
      <*  submit "Add/modify session variable"


-- | Our definition of horizontal form.
horizontal :: BootstrapFormLayout
horizontal = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)


-- | Our definition of submit button.
submit :: MonadHandler m => Text -> AForm m ()
submit t = bootstrapSubmit (BootstrapSubmit t "btn-primary" [])


-- | Retrieve the session ID from the cookie.
getSessionId :: Handler (Maybe Text)
getSessionId = lookupCookie sessionCookieName
