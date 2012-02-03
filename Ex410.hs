import Test.QuickCheck

cmap' f acc [] = acc
cmap' f acc (x:xs) = f acc x (\y -> cmap' f y xs)
  
cmap f l = cmap' (\acc x g -> (f x):(g acc)) [] l

prop_cmap_equiv_map l = cmap id l == map id l

cfilter' f acc [] = acc
cfilter' f acc (x:xs) = f acc x (\y -> cfilter' f y xs)

cfilter f l = cfilter' (\acc x g ->
                         if (f x) then x:(g acc)
                         else (g acc)) [] l
              
prop_cfilter_equiv_filter l = filter odd l == cfilter odd l

main = do
    quickCheck (prop_cmap_equiv_map :: [Integer] -> Bool)
    quickCheck (prop_cfilter_equiv_filter :: [Integer] -> Bool)

