let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let Locale = ../SubcommandConfig/Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ../SubcommandConfig/Source.dhall

let Config =
      { verbosity : Verbosity
      , colourOutput : ColourOutput
      , cacheDirectory : Optional Text
      , locale : Optional Locale
      , sources : NonEmpty.Type Source
      }

in  Config : Type
