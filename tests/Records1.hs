module Records1 where

data Int

data Records = Records { field1 :: Int, field2 :: Int }

(+) = (+)
sum x = field1 x + field2 x
