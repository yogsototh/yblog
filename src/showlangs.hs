import Config (langs)

main :: IO ()
main = mapM_ putStrLn langs
