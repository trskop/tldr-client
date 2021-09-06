-- | Configuration file for tldr-client.

let NonEmpty = ./tldr/NonEmpty/package.dhall

let SubcommandConfig = ./tldr/SubcommandConfig/package.dhall

let home = env:HOME as Text

let dataDir = env:XDG_DATA_HOME as Text ? "${home}/.local/share"

let userPages =
      { name = "user-tldr-pages"
      , format = SubcommandConfig.SourceFormat.TldrPagesWithoutIndex
      , location = SubcommandConfig.SourceLocation.Local "${dataDir}/tldr"
      }

in  SubcommandConfig::{
    , sources = NonEmpty.singleton SubcommandConfig.Source userPages
    }
