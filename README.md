ExceptionMailer
=================

Haskell module to send an notification email in the event of an uncaught exception

To use, simply call the setup routine with the desired "from" and "to" addresses.
For example,

    import System.ExceptionMailer

    main = do
        setupExceptionMailer (mkAddress "My Program" "noreply@example.com")
                             (mkAddress "Sysadmin" "sysadmin@example.com")

