let SourceFormat = ./SourceFormat.dhall

let SourceLocation = ./SourceLocation.dhall

let UnnamedSource = { format : SourceFormat, location : SourceLocation }

in  UnnamedSource : Type
