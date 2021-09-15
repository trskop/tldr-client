let Locale = ./Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ./Source.dhall

let Config =
      { cacheDirectory : Optional Text
      , locale : Optional Locale
      , sources : NonEmpty.Type Source
      , prefixes : NonEmpty.Type Text
      }

in  Config : Type
