let Locale = ./Locale.dhall

let Language = ./Language.dhall

let Country = ./Country.dhall

let defaultLocale = { language = Language.EN, country = None Country }

in  defaultLocale : Locale
