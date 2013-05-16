{-# LANGUAGE OverloadedStrings #-}

-- | Module to catch uncaught exceptions and send a notification email
module System.ExceptionMailer
    ( exceptionMailerTag
    , setupExceptionMailer, setupExceptionMailer', setupExceptionMailer_adv
    , mkAddress
    , mailError

    -- * Re-exported for convenience
    , Address
    ) where

import Prelude hiding (catch)
import System.Environment (getProgName)
import Data.String (fromString)
import Data.Maybe
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Exception (SomeException, catch)
import GHC.Conc (setUncaughtExceptionHandler)
import Network.Mail.Mime
import System.Log.Logger (errorM)

-- | String tag used for logging to "System.Log.Logger"
exceptionMailerTag :: String
exceptionMailerTag = "ExceptionMailer"

-- | Setup the global exception notifier.  This will catch any otherwise uncaught exceptions and send an email to the
-- given address.
--
-- For example,
--
-- > setupExceptionMailer (mkAddress "My Program" "noreply@example.com")
-- >                        (mkAddress "Sysadmin" "sysadmin@example.com")
setupExceptionMailer :: Address -- ^ Make the email appear to be from this address
                     -> Address -- ^ Send the email to here
                     -> Maybe String -- ^ Subject
                     -> String -- ^ Prefix to put in the email head
                     -> IO ()
setupExceptionMailer from to subj pre =
    setupExceptionMailer_adv from to subj pre (\_ -> return ())

-- | Convenience version of 'setupExceptionMailer' that just accepts the email addresses
setupExceptionMailer' :: String -- ^ Make the email appear to be from this address
                      -> String -- ^ Send the email to here
                      -> Maybe String -- ^ Subject
                      -> String -- ^ Prefix to put in the email head
                      -> IO ()
setupExceptionMailer' from to subj pre = setupExceptionMailer (Address Nothing $ fromString from) (Address Nothing $ fromString to) subj pre

-- | Setup the global exception notifier.  Like 'setupExceptionMailer' but allows a
-- custom action after the email is send
setupExceptionMailer_adv :: Address -- ^ Make the email appear to be from this address
                         -> Address -- ^ Send the email to here
                         -> Maybe String -- ^ Subject
                         -> String -- ^ Prefix to put in the email head
                         -> (SomeException -> IO ())
                         -> IO ()
setupExceptionMailer_adv from to subj pre action =
    setUncaughtExceptionHandler $ \e -> do
      emailException from to subj pre e
      action e

-- | Helper function to convert a name and email address into a proper 'Address'
mkAddress :: String -> String -> Address
mkAddress name email = Address (Just $ fromString name) $ fromString email

-- | Send an error email.  Exported so that it may be re-used from your own exception handling routines
mailError :: Address -> Address -> Maybe String -> String -> IO ()
mailError from to subj msg = do
  prog <- getProgName
  m <- simpleMail' from to (fromString $ fromMaybe "Exception Mailer" subj)
                 (LT.concat ["Program: ", fromString $ prog ++ "\n"
                            ,"Exception:\n", fromString msg])
  renderSendMail m

emailException :: Show a => Address -> Address -> Maybe String -> String -> a -> IO ()
emailException from to subj pre e = do
  errorM exceptionMailerTag $ "Uncaught exception.  emailing ("++
            show (addressEmail to)++")  : "++show e
  catch (mailError from to subj (pre ++ show e))
        (\e2 -> errorM exceptionMailerTag $ "Unable to send email : "++show (e2 :: SomeException))
  return ()

-- simpleMail' :: Monad m => Address -> Address -> Data.Text.Internal.Text -> LT.Text -> m Mail
simpleMail' from to subject plainBody =
    return Mail {
          mailFrom = from
        , mailTo   = [to]
        , mailCc   = []
        , mailBcc  = []
        , mailHeaders = [ ("Subject",  subject) ]
        , mailParts =
            [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
            $ LT.encodeUtf8 plainBody
            ]]
        }
