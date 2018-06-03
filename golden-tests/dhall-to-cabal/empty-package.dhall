  ./dhall/defaults/Package.dhall 
⫽ { name =
      "Name"
  , version =
      ./dhall/types/Version/v.dhall  "1"
  , executables =
      [ { name =
            "foo"
        , executable =
            λ(config : ./dhall/types/Config.dhall)
          → ./dhall/defaults/Executable.dhall
          ⫽ { main-is = "Main.hs" }
        }
      ]
  }
