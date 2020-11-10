module Cipher
  ( encrypt
  , decrypt
  , generateHashSalt
  , hash
  )
where

import Control.Exception

import qualified Crypto.Data.Padding as Padding (pad, Format(..))
import qualified Crypto.Simple.CTR as C
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as E
import qualified System.Entropy as Entropy

import Types

-- |Encrypts given PlainValue using given EncryptionKey.
encrypt :: EncryptionKey -> PlainValue -> IO EncryptedValue
encrypt key message = C.encrypt pkey (Encoding.encodeUtf8 message)
  where pkey = Padding.pad (Padding.PKCS7 32) (Char8.pack $ T.unpack key)

-- |Decrypts an EncryptedValue using given EncryptionKey.
decrypt :: EncryptionKey -> EncryptedValue -> IO PlainValue
decrypt key message = do
  decryptedMessage <- C.decrypt pkey message
  let result = Encoding.decodeUtf8' decryptedMessage
  case (result :: Either E.UnicodeException PlainValue) of
    Left e -> fail "Encryption key is invalid for current database. Please use the correct key."
    Right value -> return value
  where pkey = Padding.pad (Padding.PKCS7 32) (Char8.pack $ T.unpack key)

-- |Computes hash of a value using given salt.
-- Use generateHashSalt to get a good random salt.
hash :: HashSalt -> T.Text -> T.Text
hash salt value =
  T.pack $ SHA.showDigest $ SHA.sha512 $ LBS.fromStrict $ Encoding.encodeUtf8 $ T.append salt value

-- |Generates a random salt to be used for hashing values.
generateHashSalt :: IO HashSalt
generateHashSalt = generateHashSalt_ 64

generateHashSalt_ :: Int -> IO HashSalt
generateHashSalt_ n = do
  let getE n = maybe (Entropy.getEntropy n) pure =<< Entropy.getHardwareEntropy n
  rbs <- getE n
  return $ Encoding.decodeUtf8 $ Base64.encode rbs
