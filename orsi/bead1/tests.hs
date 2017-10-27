
tests =
    (
        [
            1.10338,
            0.547482,
            0.439029,
            0.362322,
            0.471192,
            0.333217,
            0.291536,
            0.312242,
            0.30196,
            0.322406,
            0.330974,
            0.319352,
            0.309879
        ],
        [
            1.07253,
            0.556815,
            0.433264,
            0.339424,
            0.435373,
            0.323479,
            0.319404,
            0.315373,
            0.304158,
            0.306819,
            0.308874,
            0.327743,
            0.320879
        ],
        [
            1.07042,
            0.548554,
            0.430011,
            0.345278,
            0.441225,
            0.327579,
            0.323728,
            0.288648,
            0.299605,
            0.319848,
            0.30849,
            0.306775,
            0.317932
        ]
    )
    
threads = [1..12] ++ [16]

testAvgs = map (\(x1,x2,x3) -> (x1+x2+x3) / 3) tupled
    where
        tupled = (\(xs1,xs2,xs3) -> zip3 xs1 xs2 xs3) tests

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
    ("Szálak száma" `column` cores)
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
    