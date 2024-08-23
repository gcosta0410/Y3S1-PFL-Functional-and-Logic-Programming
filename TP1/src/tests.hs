import Polynomials (Polynomial, addPolynomials, addPolynomials', multiplyPolynomials, multiplyPolynomials', normalize, normalize', differentiatePolynomial, parsePolynomial, convertPolynomialToString)
import Test.QuickCheck

commutativeSum :: Polynomial -> Polynomial -> Bool
commutativeSum p1 p2 = addPolynomials' p1 p2 == addPolynomials' p2 p1


commutativeMultiplication :: Polynomial -> Polynomial -> Bool
commutativeMultiplication p1 p2 = multiplyPolynomials' p1 p2 == multiplyPolynomials' p2 p1

associativeSum :: Polynomial -> Polynomial -> Polynomial ->  Bool
associativeSum p1 p2 p3 = addPolynomials' (addPolynomials' p1 p2) p3 == addPolynomials' p1 (addPolynomials' p2 p3)

associativeMultiplication :: Polynomial -> Polynomial -> Polynomial ->  Bool
associativeMultiplication p1 p2 p3 = multiplyPolynomials' (multiplyPolynomials' p1 p2) p3 == multiplyPolynomials' p1 (multiplyPolynomials' p2 p3)

identitySum :: Polynomial -> Bool
identitySum p = addPolynomials' p [] == normalize' p && addPolynomials' p [(0,[])] == normalize' p

identityMultiplication :: Polynomial ->  Bool
identityMultiplication p = multiplyPolynomials' p [(1,[])] == normalize' p

nullMultiplication :: Polynomial ->  Bool
nullMultiplication p = multiplyPolynomials' p [] == [] && multiplyPolynomials' p [(0,[])] == []

quickCheckTests :: IO ()
quickCheckTests = 
         do putStr "Testing the commutative sum property: addPolynomials' p1 p2 == addPolynomials' p2 p1  ";
            quickCheck commutativeSum;
            putStr "Testing the commutative multiplication property: mulitplyPolynomials' p1 p2 == mulitplyPolynomials' p2 p1  ";
            quickCheck (withMaxSuccess 60  commutativeMultiplication);
            putStr "Testing the associative sum property: addPolynomials' (addPolynomials' p1 p2) p3 == addPolynomials' p1 (addPolynomials' p2 p3)  ";
            quickCheck associativeSum;
            putStr "Testing the associative multiplication property: multiplyPolynomials' (multiplyPolynomials' p1 p2) p3 == multiplyPolynomials' p1 (multiplyPolynomials' p2 p3)  ";
            quickCheck (withMaxSuccess 30  associativeMultiplication);
            putStr "Testing the sum identity property:  addPolynomials' p [] == normalize' p and addPolynomials' p [(0,[])] == normalize' p  ";
            quickCheck identitySum;
            putStr "Testing the multiplication identity property: multiplyPolynomials' p [(1,[])] == normalize' p  ";
            quickCheck identityMultiplication;
            putStr "Testing the null multiplication property: multiplyPolynomials' p [] == [] and multiplyPolynomials' p [(0,[])] == []  ";
            quickCheck nullMultiplication;

generalTests :: IO ()
generalTests = do putStrLn "";
                  putStrLn "Testing addPolynomials (7xy^3 + z - 8 + 2 -x) (-xy^3 +3*z + 8w)";
                  putStrLn (addPolynomials "7xy^3 + z - 8 + 2 -x" "-xy^3 +3*z + 8w");
                  putStrLn "";
                  putStrLn "Testing multiplyPolynomials (7xy^3 + z - 8 + 2 -x) (-xy^3 +3*z + 8w)";
                  putStrLn (multiplyPolynomials "7xy^3 + z - 8 + 2 -x" "(-xy^3 +3*z + 8w)");
                  putStrLn "";
                  putStrLn "Testing differentiatePolynomial -7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z x";
                  putStrLn (differentiatePolynomial "-7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z" 'x');
                  putStrLn "";
                  putStrLn "Testing differentiatePolynomial -7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z y";
                  putStrLn (differentiatePolynomial "-7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z" 'y');
                  putStrLn "";
                  putStrLn "Testing differentiatePolynomial -7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z w";
                  putStrLn (differentiatePolynomial "-7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z" 'w');
                  putStrLn "";
                  putStrLn "Testing normalize 0x^2 -0 + 14*x - 7x -7x +2xy +3*x -15x*y + 24 - 1";
                  putStrLn (normalize "0x^2 -0 + 14*x - 7x -7x +2xy +3*x -15x*y + 24 - 1");
                  putStrLn "";
                  putStrLn "Testing parsePolynomial 0x^2 -0 + 14*x - 7x -7x +2xy +3*x -15x*y + 24 - 1";
                  print (parsePolynomial "0x^2 -0 + 14*x - 7x -7x +2xy +3*x -15x*y + 24 - 1");
                  putStrLn "";
                  putStrLn "Testing parsePolynomial 3*x + -13*x*y + 23";
                  print (parsePolynomial "3*x + -13*x*y + 23");
                  putStrLn "";
                  putStrLn "Testing convertPolynomialToString [(0,[('x',2)]),(0,[]),(14,[('x',1)]),(-7,[('x',1)]),(-7,[('x',1)]),(2,[('x',1),('y',1)]),(3,[('x',1)]),(-15,[('x',1),('y',1)]),(24,[]),(-1,[])]";
                  print (convertPolynomialToString [(0,[('x',2)]),(0,[]),(14,[('x',1)]),(-7,[('x',1)]),(-7,[('x',1)]),(2,[('x',1),('y',1)]),(3,[('x',1)]),(-15,[('x',1),('y',1)]),(24,[]),(-1,[])]);
                  putStrLn "";
                  putStrLn "Testing convertPolynomialToString [(3,[('x',1)]),(-1,[('x',2),('y',1)]),(-23,[]), (2,[('z',12)])]";
                  print (convertPolynomialToString [(3,[('x',1)]),(-1,[('x',2),('y',1)]),(-23,[]), (2,[('z',12)])]);