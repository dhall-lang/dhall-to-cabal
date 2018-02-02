    let OS = constructors ./dhall/types/OS 

in  let Arch = constructors ./dhall/types/Arch 

in  let Compiler = constructors ./dhall/types/Compiler 

in    ./dhall/empty-package.dhall 
    ⫽ { library =
          [ [ { body =
                    ./dhall/defaults/Library.dhall 
                  ⫽ { buildable = False, exposed-modules = [ "Bar" ] }
              , guard =
                    λ(config : ./dhall/types/Config )
                  →     config.os (OS.Linux {=}) == config.arch (Arch.Mips {=})
                    &&  (       if    config.os (OS.Linux {=})
                                then  config.arch (Arch.X86_64 {=})
                          
                          else  config.arch (Arch.PPC {=})
                        )
              }
            , { body =
                    ./dhall/defaults/Library.dhall 
                  ⫽ { exposed-modules = [ "Foo" ] }
              , guard =
                    λ(config : ./dhall/types/Config )
                  →     config.arch (Arch.Mips {=})
                    &&  config.impl
                        (Compiler.GHC {=})
                        (majorBoundVersion [ +8, +2 ])
              }
            , { body =
                    ./dhall/defaults/Library.dhall 
                  ⫽ { exposed-modules = [ "Hello" ] }
              , guard =
                  λ(config : ./dhall/types/Config ) → config.os (OS.Linux {=})
              }
            ]
          ] : Optional (./dhall/types/Guarded  ./dhall/types/Library )
      , name =
          "test"
      , version =
          [ +1 ]
      }
