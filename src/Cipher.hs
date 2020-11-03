module Cipher
  ( encrypt
  , decrypt
  , hash
  )
where

import Control.Exception

import qualified Crypto.Simple.CTR as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Crypto.Data.Padding (pad, Format(PKCS7))

import Types

encrypt :: EncryptionKey -> PlainValue -> IO EncryptedValue
encrypt key message = C.encrypt pkey (encodeUtf8 message) where pkey = pad (PKCS7 32) (pack key)

decrypt :: EncryptionKey -> EncryptedValue -> IO PlainValue
decrypt key message = do
  decryptedMessage <- C.decrypt pkey message
  let result = decodeUtf8' decryptedMessage
  case (result :: Either UnicodeException PlainValue) of
    Left e -> fail "Encryption key is invalid for current database. Please use the correct key."
    Right value -> return value
  where pkey = pad (PKCS7 32) (pack key)

hash :: T.Text -> String
hash bs = SHA.showDigest $ SHA.sha512 $ LBS.fromStrict $ encodeUtf8 bs
