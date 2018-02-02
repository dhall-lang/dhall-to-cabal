  ./dhall/empty-package.dhall 
⫽ { package =
      { name = "Name", version = [ +1 ] }
  , executables =
      [ { name =
            "foo"
        , executable =
            [ { guard =
                  λ(config : ./dhall/types/Config ) → True
              , body =
                  ./dhall/defaults/Executable.dhall 
              }
            ]
        }
      ]
  }
