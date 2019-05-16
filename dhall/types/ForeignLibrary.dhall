  ./BuildInfo.dhall
⩓ { type :
      ./ForeignLibType.dhall
  , options :
      List ./ForeignLibOption.dhall
  , lib-version-info :
      Optional { current : Natural, revision : Natural, age : Natural }
  , lib-version-linux :
      Optional ./Version.dhall
  , mod-def-files :
      List Text
  }
