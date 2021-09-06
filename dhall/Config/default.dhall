let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let Locale = ../SubcommandConfig/Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ../SubcommandConfig/Source.dhall

let SourceFormat = ../SubcommandConfig/SourceFormat.dhall

let SourceLocation = ../SubcommandConfig/SourceLocation.dhall

let Config = ./Type.dhall

let tldrPages =
      let tldrPagesUrls
          : NonEmpty.Type Text
          = { head = "https://tldr.sh/assets/tldr.zip"
            , tail =
              [ "https://raw.githubusercontent.com/tldr-pages/tldr-pages.github.io/master/assets/tldr.zip"
              ]
            }
      
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
