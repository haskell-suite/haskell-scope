module Records2 where

data Int

data Records = Records { field1 :: Int, field2 :: Int }

(+) = (+)
sum Records{field1=x, field2=y} = x + y
