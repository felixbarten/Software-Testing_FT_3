Euler 9: Special Pythagorean triplet
===========================

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.


> -- x^2 + y^2 = z^2 
> pythagoreanTriples n = [( x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x <= y, (x^2) + (y^2) == (z^2), x < y && y < z]

