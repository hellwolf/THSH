#!/usr/bin/env thsh

sumSales = lineReadFn
           (\ (_ :: String, price, quantity) s -> let s' = s + price * quantity
                                                  in (s', Nothing))
           (\ s -> Just (show s ++ "\n"))
           (0.0 :: Float)

__main__ = [thsh|\
echo -n "Sum of the sales: "; {
# Read (product :: String, price :: Float, quanity :: Float)
«fn sumSales» <<EOF
("apple",  1.2, 100.2)
("orange", 2.0, 34.2)
("pear", 1.2, 62.3)
EOF
} | tail -n1
|]
