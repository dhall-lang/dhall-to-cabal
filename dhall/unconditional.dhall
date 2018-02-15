  λ(VersionRange : Type)
→     let unconditional
          : ∀(A : Type) → A → ./types/Guarded.dhall  VersionRange A
          =   λ(A : Type)
            → λ(a : A)
            → [ { guard =
                    λ(_ : ./types/Config.dhall  VersionRange) → True
                , body =
                    a
                }
              ]
  
  in  let executable
          :   ∀(name : Text)
            → ∀(executable : ./types/Executable.dhall  VersionRange)
            → { name :
                  Text
              , executable :
                  ./types/Guarded.dhall 
                  VersionRange
                  (./types/Executable.dhall  VersionRange)
              }
          =   λ(name : Text)
            → λ(executable : ./types/Executable.dhall  VersionRange)
            → { name =
                  name
              , executable =
                  unconditional
                  (./types/Executable.dhall  VersionRange)
                  executable
              }
  
  in  let library
          :   ∀(library : ./types/Library.dhall  VersionRange)
            → Optional
              ( ./types/Guarded.dhall 
                VersionRange
                (./types/Library.dhall  VersionRange)
              )
          =   λ(library : ./types/Library.dhall  VersionRange)
            → [ unconditional (./types/Library.dhall  VersionRange) library
              ] : Optional
                  ( ./types/Guarded.dhall 
                    VersionRange
                    (./types/Library.dhall  VersionRange)
                  )
  
  in  let test-suite
          :   ∀(name : Text)
            → ∀(test-suite : ./types/TestSuite.dhall  VersionRange)
            → { name :
                  Text
              , test-suite :
                  ./types/Guarded.dhall 
                  VersionRange
                  (./types/TestSuite.dhall  VersionRange)
              }
          =   λ(name : Text)
            → λ(test-suite : ./types/TestSuite.dhall  VersionRange)
            → { name =
                  name
              , test-suite =
                  unconditional
                  (./types/TestSuite.dhall  VersionRange)
                  test-suite
              }
  
  in  { executable = executable, library = library, test-suite = test-suite }
