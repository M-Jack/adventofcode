module Day01Spec where

import Day01
import SpecHelper


newtype ParenthesisString = ParenthesisString { unwrapString :: String}
    deriving (Show)
genParenthesis :: Gen Char
genParenthesis = elements "()"

genParenthesisString :: Gen String
genParenthesisString = listOf genParenthesis

instance Arbitrary ParenthesisString where
    arbitrary = ParenthesisString <$> genParenthesisString


prop_sum :: ParenthesisString -> ParenthesisString -> Bool
prop_sum ps1 ps2 = elevator s1 + elevator s2 == elevator (s1 ++ s2)
    where s1 = unwrapString ps1
          s2 = unwrapString ps2

prop_limit :: ParenthesisString -> Bool
prop_limit ps = elevator s <= length s && elevator s >= - length s
    where s = unwrapString ps

prop_reverse :: ParenthesisString -> Bool
prop_reverse ps = elevator s == elevator (reverse s)
    where s = unwrapString ps


spec :: Spec
spec = do
    describe "Day01" $ do
       it "fullfill the sum property" $ property prop_sum
       it "fullfill the limit property" $ property prop_limit
       it "fullfill the reverse property" $ property prop_reverse