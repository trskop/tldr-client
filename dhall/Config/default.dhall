let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let Locale = ../SubcommandConfig/Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ../SubcommandConfig/Source.dhall

let SourceFormat = ../SubcommandConfig/SourceFormat.dhall

let SourceLocation = ../SubcommandConfig/SourceLocation.dhall

let Config = ./Type.dhall

let tldrPages =
      let -- URL taken from
          -- <https://github.com/tldr-pages/tldr/blob/v2.2/CLIENT-SPECIFICATION.md>
          tldrPagesUrls
          : NonEmpty.Type Text
          = NonEmpty.singleton
              "https://github.com/tldr-pages/tldr/releases/latest/download/tldr.zip"

      in  NonEmpty.singleton
            Source
            { name = "tldr-pages"
            , format = SourceFormat.TldrPagesWithIndex
            , location = SourceLocation.Remote tldrPagesUrls
            }

let default =
      { verbosity = Verbosity.Normal
      , colourOutput = ColourOutput.Auto
      , cacheDirectory = None Text
      , locale = None Locale
      , sources = tldrPages
      }

in  default : Config
