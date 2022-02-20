module Utils.Maybe where

justOrDefault :: Maybe a -> a -> a
justOrDefault Nothing defaultValue = defaultValue
justOrDefault (Just value) _ = value
