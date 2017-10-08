-- The order of the where statements
-- does not matter

-- 1
foo1 = x * 3 + y
  where x = 3
        y = 1000

-- 2
foo2 = x * 5
  where x = 10 * 5 + y
        y = 10

-- 3
foo3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10