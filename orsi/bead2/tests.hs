
tests =
    [
        (0.061376, 0.0608715, 0.0715913),
        (0.0611922, 0.0361456, 0.0631445),
        (0.0307498, 0.0327922, 0.357019),
        (0.0527187, 0.278622, 0.139124)
    ]

threads = [0,1,2,4]

testAvgs = map (\(x1,x2,x3) -> (x1+x2+x3) / 3) tests

testResult [] [] = []
testResult (t:ts) (avg:avgs) = (t,avg,fromInteger t * avg) : testResult ts avgs

testResults = testResult threads testAvgs

backToList [] = ([],[],[])
backToList ((a,b,c):xs) =
    let (x,y,z) = backToList xs in
         (a:x,b:y,c:z)

roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
        
testResults' = let (cores, avg, cost) = backToList testResults in
               (cores, mapRound avg, mapRound cost)
               where
                mapRound = map $ roundTo 3

latex (cores, avg, cost) = (\a b c -> (a,b,c))
    ("Bladek száma" `column` cores)
    ("Futásidő (s)" `column` avg)
    ("Összköltség " `column` cost)
    where
        column name vals = name ++ foldl (\state elem -> state ++ " & " ++ show elem) "" vals ++ "\\\\"
        
main :: IO ()
main = do
    let (a1,a2,a3) = latex testResults'
    putStrLn a1
    putStrLn a2
    putStrLn a3
    