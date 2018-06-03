  ./BuildInfo.dhall 
⩓ { type :
      < Shared : {} | Static : {} >
  , options :
      List < Standalone : {} >
  , lib-version-info :
      Optional { age : Natural, current : Natural, revision : Natural }
  , lib-version-linux :
      Optional ./Version.dhall 
  , mod-def-files :
      List Text
  }
