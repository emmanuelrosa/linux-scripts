{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : An i3blocks block which displays the battery status.
Copyright   : (c) 2019 Emmanuel Rosa
License     : MIT
Maintainer  : emmanuelrosa@protonmail.com
Stability   : experimental
Portability : non-portable (uses Linux /sys interface)
-}

import Prelude hiding (FilePath)
import Turtle
import Data.List
import Control.Applicative


{- Represents the battery status.
 - The `Int` is the battery's charge
 - The `Bool` is true when AC power is plugged in, otherwise it's false
 -}
data Status = 
    Charging Int
    | Discharging Int
    | Charged Bool deriving (Show)




main :: IO ()
main = (stdout . display) status where 
    chargeNow :: Num a => Shell (Maybe a)
    chargeNow = readNumber "/sys/class/power_supply/BAT0/charge_now"

    chargeFull :: Num a => Shell (Maybe a)
    chargeFull = readNumber "/sys/class/power_supply/BAT0/charge_full"

    charge :: Integral a => Shell (Maybe a)
    charge = liftA2 getCharge chargeNow chargeFull

    status :: Shell (Either Text Status)
    status = (liftA3 getStatus) charge ac status'

    status' :: Shell Text
    status' = fmap lineToText $ input "/sys/class/power_supply/BAT0/status"

    ac :: Shell (Either Text Bool)
    ac = fmap f ac' where
        f (Just n)
            | n == 0 = Right False
            | n == 1 = Right True
            | otherwise = Left (format ("Invalid AC status " %d) n)

        f Nothing = Left "Unable to parse AC status"

    ac' :: Num a => Shell (Maybe a)
    ac' = readNumber "/sys/class/power_supply/AC/online"




{- Renders the battery status in a textual format.
 - Ready to be consumed by i3blocks :)
 -}
display :: Shell (Either Text Status) -> Shell Line
display = fmap (unsafeTextToLine . display')

display' :: Either Text Status -> Text
display' (Left t) = 
    format ("ERROR: '" %s% "'") t

display' (Right (Charging charge)) = 
    format (d% "% \xf5e7") charge

display' (Right (Discharging charge)) = 
    format (d% "% " %s) charge icon where
        icon
            | charge <= 25 = "\xf243"
            | charge >= 70 = "\xf241"
            | otherwise = "\xf242"

display' (Right (Charged True)) =
     "\xf1e6"

display' (Right (Charged False)) =
     "\xf240"



{- Processes the charge amount, the AC status, and the battery status text 
 - to produce a unified battery status. 
 -}
getStatus :: Maybe Int -> (Either Text Bool) -> Text -> Either Text Status
getStatus (Just charge) (Right isPlugged) t =
    getStatus' charge isPlugged t

getStatus (Just _) (Left err) _ = Left err

getStatus Nothing  _ _ =
    Left "Unable to calculate battery charge"

getStatus' :: Int -> Bool -> Text -> Either Text Status
getStatus' charge isPlugged "Charging" =
    Right $ Charging charge 

getStatus' charge isPlugged "Full" =
    Right $ Charged isPlugged 

getStatus' charge _ "Discharging" = Right $ Discharging charge 

getStatus' _ _ t = Left t




{- Calculates the current battery charged percentage.
 -}
getCharge :: (RealFrac a, Integral b) => Maybe a -> Maybe a -> Maybe b
getCharge now full = liftA2 getCharge' now full 

getCharge' :: (RealFrac a, Integral b) => a -> a -> b
getCharge' now full = floor $ now / full * 100




{- Reads a line of text from a file and attempts to convert it to a number.
 -}
readNumber :: Num a => FilePath -> Shell (Maybe a)
readNumber path = 
    fmap ((fmap fst) . uncons)
        $ fmap ((match decimal) . lineToText) 
            $ input path
