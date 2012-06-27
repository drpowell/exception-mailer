import System.ExceptionMailer

-- For some reason this example only works when compiled, not when intepreted 
main = do
    setupExceptionMailer (mkAddress "My Program" "noreply@example.com")
                         (mkAddress "Sysadmin" "sysadmin@example.com")

    -- Unexpected runtime error
    let badList = [] :: [Int]
    print $ head badList

