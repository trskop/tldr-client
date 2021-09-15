let Source = ./Source.dhall

let UnnamedSource = ./UnnamedSource.dhall

let Prelude =
      https://prelude.dhall-lang.org/v19.0.0/package.dhall sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let sourcesFromMap =
      Prelude.List.map
        (Prelude.Map.Entry Text UnnamedSource)
        Source
        ( λ(entry : Prelude.Map.Entry Text UnnamedSource) →
            entry.mapValue ⫽ { name = entry.mapKey }
        )

in  sourcesFromMap : Prelude.Map.Type Text UnnamedSource → List Source
