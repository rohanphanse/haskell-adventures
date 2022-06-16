-- Load file into GHCi with :load newtons-method.hs

-- Given a value of x, function f(x), its derivative f'(x), and number of iterations, approximate a root (x-intercept) of f(x) with Newton's method
newtonsMethod :: (RealFloat a, Integral b) => (a -> a) -> (a -> a) -> a -> b -> a
newtonsMethod _ _ x 0 = x
newtonsMethod f f' x iterations = newtonsMethod f f' (x - (f x) / (f' x)) (iterations - 1) 

approximations :: (RealFloat a, Integral b) => (a -> a) -> (a -> a) -> a -> b -> [a]
approximations f f' x iterations = map (\i -> newtonsMethod f f' x i) [0..iterations]

-- Test 1: f(x) = x^2 - 4, expect root = 2.0
f1 x = x**2 - 4
f1' x = 2*x

-- Test 2: f(x) = e^x + ln(x), expect root = 0.27
f2 x = exp x + log x
f2' x = exp x + 1/x

main = do
    print ("Newton's method!")
    -- Test 1
    print (newtonsMethod f1 f1' 1 10)
    print (approximations f1 f1' 1 10)
    -- Test 2
    print (approximations f2 f2' 0.1 10)



