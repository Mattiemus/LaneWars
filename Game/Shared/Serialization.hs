module Game.Shared.Serialization where

import Data.Serialize
import Data.Serialize.Get
import Data.ByteString as BS

-- |Decodes every value from a byte string, returning the list of values
-- read. Will stop at and return the first error encountered.
decodeAll :: Serialize a => ByteString -> Either String [a]
decodeAll bytes = decodeAll' bytes []
    where
        decodeAll' :: Serialize a => ByteString -> [a] -> Either String [a]
        decodeAll' bytes vals = case runGetState get bytes 0 of
                Left err -> Left err
                Right (val, remBytes) -> if BS.null remBytes
                    then Right (val:vals)
                    else decodeAll' remBytes (val:vals)
                    