import Unif

testL1 = List [Var "" "x", Var "" "y", Var "" "z"]
testL2 = List [Con "1", Con "2", Con "3"]
testX = Var "" "X"
testUnifL = [testL1, testL2]
testUnifX = [testX, testX]

testCns1 = Cons (Var "" "x") (Var "" "xs")
testUnifC1 = [testL1, testCns1]
testUnifC2 = [testL2, testCns1]

abu = Var "" "A"
by = Var "" "B"
xy = Var "" "X"
ybu = Var "" "Y"
zy = Var "" "Z"
hoge = Con "hoge"
before :: [(Term String String, Term String String)]
before = [(xy, abu), (abu, ybu), (by, zy), (hoge, by)]
