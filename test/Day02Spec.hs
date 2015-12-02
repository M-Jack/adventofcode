module Day02Spec where

import Day02
import SpecHelper

prop_order :: (Int, Int, Int) -> Bool
prop_order (x,y,z) = presentFolding (x,y,z) == presentFolding (y,z,x)

prop_inverse :: (Int, Int, Int) -> Bool
prop_inverse (x,y,z) = (x,y,z) == dimensionToTuple (show x ++ "x" ++ show y ++ "x" ++ show z)

spec :: Spec
spec = do
    describe "presentFolding" $ do
            it "return 7" $ presentFolding (1,1,1) `shouldBe` 7
            it "return 58" $ presentFolding (2,3,4) `shouldBe` 58
            it "return 7" $ presentFolding (1,1,10) `shouldBe` 43
            it "fullfill the order property" $ property prop_order
    describe "dimensionToTuple" $ do
            it "works" $ do dimensionToTuple "1x1x1" `shouldBe` (1,1,1)
            it "fullfill the inverse property" $ property prop_inverse
    describe "ribbonKnotting" $ do 
            it "works" $ do ribbonKnotting (2,3,4) `shouldBe` 34
            it "works" $ do ribbonKnotting (1,1,10) `shouldBe` 14