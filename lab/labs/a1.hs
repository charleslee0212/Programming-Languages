module A1 where
-- Charles Lee

-- Function creates a quadratic equation
-- consumes 4 parameters a b c x all num
-- produces the solution regarding the parameters
-- This will return a number
quadratic a b c x = a + b * x + c * x**2

-- Function scales the vector
-- consumes a number and a tuple
-- produces a vector that has been scaled
-- This will return a tuple
scaleVector a (b,c) = (a * b, a * c)

-- Function defines the distance between two 3-dimensional points
-- consumes two tuples
-- produces the cartesian distance
-- This will return a number
tripleDistance (a,b,c) (x,y,z) = ((x - a)**2 + (y - b)**2 + (z - c)**2)**(1/2)
