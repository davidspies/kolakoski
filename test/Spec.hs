import           Lib
import           Test.Hspec
import           Test.QuickCheck

prop_correctLength :: Starts -> Property
prop_correctLength ts = startsLen ts === toInteger (length $ fromStarts ts)

prop_correctTerm :: NonNegative Int -> Property
prop_correctTerm (NonNegative n) = getKol (toInteger n) === kolakoski !! n

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Kolakoski" $ do
  it "should get lengths right" $ property prop_correctLength
  it "should get terms right" $ property prop_correctTerm
