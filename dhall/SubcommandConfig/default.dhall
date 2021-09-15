let Locale = ./Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let default =
      { cacheDirectory = None Text
      , locale = None Locale
      , prefixes = NonEmpty.singleton Text "command-wrapper"
      }

in  default
