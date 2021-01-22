import ClassyPrelude
import Test.QuickCheck

main :: IO ()
main = quickCheck (const True)