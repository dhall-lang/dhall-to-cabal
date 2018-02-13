  ./dhall/defaults/Package.dhall 
⫽ { name =
      "Name"
  , version =
      [ +1 ]
  , executables =
      [ { name =
            "foo"
        , executable =
            [ { guard =
                  λ(config : ./dhall/types/Config.dhall ) → True
              , body =
                  ./dhall/defaults/Executable.dhall 
              }
            ]
        }
      ]
  }
