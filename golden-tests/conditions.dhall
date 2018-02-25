    let stdlib = ./dhall/stdlib.dhall 

in  let OS = constructors ./dhall/types/OS.dhall 

in  let Arch = constructors ./dhall/types/Arch.dhall 

in  let Compiler = constructors ./dhall/types/Compiler.dhall 

in    ./dhall/defaults/Package.dhall 
    ⫽ { library =
          [   λ(config : ./dhall/types/Config.dhall )
            → 
                      ./dhall/defaults/Library.dhall 
                    ⫽ (       if            config.os (OS.Linux {=})
                                        ==  config.arch (Arch.Mips {=})
                                    &&  (       if    config.os (OS.Linux {=})
                                                then  config.arch
                                                      (Arch.X86_64 {=})
                                          
                                          else  config.arch (Arch.PPC {=})
                                        )
                              then  { buildable =
                                        False
                                    , exposed-modules =
                                        [ "Bar" ]
                                    }
                        
                        else  { buildable =
                                  True
                              , exposed-modules =
                                  [] : List Text
                              }
                      )
                    ⫽ (       if        config.arch (Arch.Mips {=})
                                    &&  config.impl
                                        (Compiler.GHC {=})
                                        (./dhall/types/VersionRange/MajorBoundVersion.dhall (./dhall/types/Version/v.dhall "8.2"))
                              then  { exposed-modules = [ "Foo" ] }
                        
                        else  { exposed-modules = [] : List Text }
                      )
                    ⫽ (       if    config.os (OS.Linux {=})
                              then  { exposed-modules = [ "Hello" ] }
                        
                        else  { exposed-modules = [] : List Text }
                      )
          ] : Optional
              (./dhall/types/Guarded.dhall  ./dhall/types/Library.dhall )
      , name =
          "test"
      , version =
          ./dhall/types/Version/v.dhall "1.0"
      }
