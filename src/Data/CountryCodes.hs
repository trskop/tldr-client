-- |
-- Module:      Data.CountryCodes
-- Description: Two-letter country codes defined in ISO 3166-1
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Two-letter country codes defined in ISO 3166-1.
module Data.CountryCodes
    ( CountryCode(..)
    , fromText
    , toString
    , values

    -- * Dhall Encoding
    , decode
    , )
  where

import Prelude (Bounded, Enum, maxBound, minBound, )

import Data.Bool (otherwise, )
import Data.Char (Char, )
import Data.Eq (Eq, )
import Data.Function ((.), )
import Data.Maybe (Maybe(Just, Nothing), )
import Data.Ord (Ord, )
import Data.String (IsString, fromString, )
import GHC.Generics (Generic, )
import Text.Show (Show, show, )

import Data.Text (Text, )
import Data.Text qualified as Text (null, uncons, )
import Dhall.Map qualified (fromList, )
import Dhall.Marshal.Decode.Extended qualified as Dhall (Decoder, enum, )

-- | 2-letter country code as defined by [ISO 3166-1 alpha-2
-- ](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2).
data CountryCode
    = AD
    | AE
    | AF
    | AG
    | AI
    | AL
    | AM
    | AO
    | AQ
    | AR
    | AS
    | AT
    | AU
    | AW
    | AX
    | AZ
    | BA
    | BB
    | BD
    | BE
    | BF
    | BG
    | BH
    | BI
    | BJ
    | BL
    | BM
    | BN
    | BO
    | BQ
    | BR
    | BS
    | BT
    | BV
    | BW
    | BY
    | BZ
    | CA
    | CC
    | CD
    | CF
    | CG
    | CH
    | CI
    | CK
    | CL
    | CM
    | CN
    | CO
    | CR
    | CU
    | CV
    | CW
    | CX
    | CY
    | CZ
    | DE
    | DJ
    | DK
    | DM
    | DO
    | DZ
    | EC
    | EE
    | EG
    | EH
    | ER
    | ES
    | ET
    | FI
    | FJ
    | FK
    | FM
    | FO
    | FR
    | GA
    | GB
    | GD
    | GE
    | GF
    | GG
    | GH
    | GI
    | GL
    | GM
    | GN
    | GP
    | GQ
    | GR
    | GS
    | GT
    | GU
    | GW
    | GY
    | HK
    | HM
    | HN
    | HR
    | HT
    | HU
    | ID
    | IE
    | IL
    | IM
    | IN
    | IO
    | IQ
    | IR
    | IS
    | IT
    | JE
    | JM
    | JO
    | JP
    | KE
    | KG
    | KH
    | KI
    | KM
    | KN
    | KP
    | KR
    | KW
    | KY
    | KZ
    | LA
    | LB
    | LC
    | LI
    | LK
    | LR
    | LS
    | LT
    | LU
    | LV
    | LY
    | MA
    | MC
    | MD
    | ME
    | MF
    | MG
    | MH
    | MK
    | ML
    | MM
    | MN
    | MO
    | MP
    | MQ
    | MR
    | MS
    | MT
    | MU
    | MV
    | MW
    | MX
    | MY
    | MZ
    | NA
    | NC
    | NE
    | NF
    | NG
    | NI
    | NL
    | NO
    | NP
    | NR
    | NU
    | NZ
    | OM
    | PA
    | PE
    | PF
    | PG
    | PH
    | PK
    | PL
    | PM
    | PN
    | PR
    | PS
    | PT
    | PW
    | PY
    | QA
    | RE
    | RO
    | RS
    | RU
    | RW
    | SA
    | SB
    | SC
    | SD
    | SE
    | SG
    | SH
    | SI
    | SJ
    | SK
    | SL
    | SM
    | SN
    | SO
    | SR
    | SS
    | ST
    | SV
    | SX
    | SY
    | SZ
    | TC
    | TD
    | TF
    | TG
    | TH
    | TJ
    | TK
    | TL
    | TM
    | TN
    | TO
    | TR
    | TT
    | TV
    | TW
    | TZ
    | UA
    | UG
    | UM
    | US
    | UY
    | UZ
    | VA
    | VC
    | VE
    | VG
    | VI
    | VN
    | VU
    | WF
    | WS
    | YE
    | YT
    | ZA
    | ZM
    | ZW
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

toString :: IsString s => CountryCode -> s
toString = fromString . show
{-# SPECIALISE toString :: CountryCode -> Text #-}
{-# INLINEABLE toString #-}

fromText :: Text -> Maybe CountryCode
fromText t
  | Just (c₀, t₀) <- Text.uncons t
  , Just (c₁, t₁) <- Text.uncons t₀
  , Text.null t₁ =
        fromChars c₀ c₁
  | otherwise = Nothing
{-# INLINEABLE fromText #-}

fromChars :: Char -> Char -> Maybe CountryCode
fromChars c₀ c₁ = case (c₀, c₁) of
    ('A', 'D') -> Just AD
    ('A', 'E') -> Just AE
    ('A', 'F') -> Just AF
    ('A', 'G') -> Just AG
    ('A', 'I') -> Just AI
    ('A', 'L') -> Just AL
    ('A', 'M') -> Just AM
    ('A', 'O') -> Just AO
    ('A', 'Q') -> Just AQ
    ('A', 'R') -> Just AR
    ('A', 'S') -> Just AS
    ('A', 'T') -> Just AT
    ('A', 'U') -> Just AU
    ('A', 'W') -> Just AW
    ('A', 'X') -> Just AX
    ('A', 'Z') -> Just AZ
    ('B', 'A') -> Just BA
    ('B', 'B') -> Just BB
    ('B', 'D') -> Just BD
    ('B', 'E') -> Just BE
    ('B', 'F') -> Just BF
    ('B', 'G') -> Just BG
    ('B', 'H') -> Just BH
    ('B', 'I') -> Just BI
    ('B', 'J') -> Just BJ
    ('B', 'L') -> Just BL
    ('B', 'M') -> Just BM
    ('B', 'N') -> Just BN
    ('B', 'O') -> Just BO
    ('B', 'Q') -> Just BQ
    ('B', 'R') -> Just BR
    ('B', 'S') -> Just BS
    ('B', 'T') -> Just BT
    ('B', 'V') -> Just BV
    ('B', 'W') -> Just BW
    ('B', 'Y') -> Just BY
    ('B', 'Z') -> Just BZ
    ('C', 'A') -> Just CA
    ('C', 'C') -> Just CC
    ('C', 'D') -> Just CD
    ('C', 'F') -> Just CF
    ('C', 'G') -> Just CG
    ('C', 'H') -> Just CH
    ('C', 'I') -> Just CI
    ('C', 'K') -> Just CK
    ('C', 'L') -> Just CL
    ('C', 'M') -> Just CM
    ('C', 'N') -> Just CN
    ('C', 'O') -> Just CO
    ('C', 'R') -> Just CR
    ('C', 'U') -> Just CU
    ('C', 'V') -> Just CV
    ('C', 'W') -> Just CW
    ('C', 'X') -> Just CX
    ('C', 'Y') -> Just CY
    ('C', 'Z') -> Just CZ
    ('D', 'E') -> Just DE
    ('D', 'J') -> Just DJ
    ('D', 'K') -> Just DK
    ('D', 'M') -> Just DM
    ('D', 'O') -> Just DO
    ('D', 'Z') -> Just DZ
    ('E', 'C') -> Just EC
    ('E', 'E') -> Just EE
    ('E', 'G') -> Just EG
    ('E', 'H') -> Just EH
    ('E', 'R') -> Just ER
    ('E', 'S') -> Just ES
    ('E', 'T') -> Just ET
    ('F', 'I') -> Just FI
    ('F', 'J') -> Just FJ
    ('F', 'K') -> Just FK
    ('F', 'M') -> Just FM
    ('F', 'O') -> Just FO
    ('F', 'R') -> Just FR
    ('G', 'A') -> Just GA
    ('G', 'B') -> Just GB
    ('G', 'D') -> Just GD
    ('G', 'E') -> Just GE
    ('G', 'F') -> Just GF
    ('G', 'G') -> Just GG
    ('G', 'H') -> Just GH
    ('G', 'I') -> Just GI
    ('G', 'L') -> Just GL
    ('G', 'M') -> Just GM
    ('G', 'N') -> Just GN
    ('G', 'P') -> Just GP
    ('G', 'Q') -> Just GQ
    ('G', 'R') -> Just GR
    ('G', 'S') -> Just GS
    ('G', 'T') -> Just GT
    ('G', 'U') -> Just GU
    ('G', 'W') -> Just GW
    ('G', 'Y') -> Just GY
    ('H', 'K') -> Just HK
    ('H', 'M') -> Just HM
    ('H', 'N') -> Just HN
    ('H', 'R') -> Just HR
    ('H', 'T') -> Just HT
    ('H', 'U') -> Just HU
    ('I', 'D') -> Just ID
    ('I', 'E') -> Just IE
    ('I', 'L') -> Just IL
    ('I', 'M') -> Just IM
    ('I', 'N') -> Just IN
    ('I', 'O') -> Just IO
    ('I', 'Q') -> Just IQ
    ('I', 'R') -> Just IR
    ('I', 'S') -> Just IS
    ('I', 'T') -> Just IT
    ('J', 'E') -> Just JE
    ('J', 'M') -> Just JM
    ('J', 'O') -> Just JO
    ('J', 'P') -> Just JP
    ('K', 'E') -> Just KE
    ('K', 'G') -> Just KG
    ('K', 'H') -> Just KH
    ('K', 'I') -> Just KI
    ('K', 'M') -> Just KM
    ('K', 'N') -> Just KN
    ('K', 'P') -> Just KP
    ('K', 'R') -> Just KR
    ('K', 'W') -> Just KW
    ('K', 'Y') -> Just KY
    ('K', 'Z') -> Just KZ
    ('L', 'A') -> Just LA
    ('L', 'B') -> Just LB
    ('L', 'C') -> Just LC
    ('L', 'I') -> Just LI
    ('L', 'K') -> Just LK
    ('L', 'R') -> Just LR
    ('L', 'S') -> Just LS
    ('L', 'T') -> Just LT
    ('L', 'U') -> Just LU
    ('L', 'V') -> Just LV
    ('L', 'Y') -> Just LY
    ('M', 'A') -> Just MA
    ('M', 'C') -> Just MC
    ('M', 'D') -> Just MD
    ('M', 'E') -> Just ME
    ('M', 'F') -> Just MF
    ('M', 'G') -> Just MG
    ('M', 'H') -> Just MH
    ('M', 'K') -> Just MK
    ('M', 'L') -> Just ML
    ('M', 'M') -> Just MM
    ('M', 'N') -> Just MN
    ('M', 'O') -> Just MO
    ('M', 'P') -> Just MP
    ('M', 'Q') -> Just MQ
    ('M', 'R') -> Just MR
    ('M', 'S') -> Just MS
    ('M', 'T') -> Just MT
    ('M', 'U') -> Just MU
    ('M', 'V') -> Just MV
    ('M', 'W') -> Just MW
    ('M', 'X') -> Just MX
    ('M', 'Y') -> Just MY
    ('M', 'Z') -> Just MZ
    ('N', 'A') -> Just NA
    ('N', 'C') -> Just NC
    ('N', 'E') -> Just NE
    ('N', 'F') -> Just NF
    ('N', 'G') -> Just NG
    ('N', 'I') -> Just NI
    ('N', 'L') -> Just NL
    ('N', 'O') -> Just NO
    ('N', 'P') -> Just NP
    ('N', 'R') -> Just NR
    ('N', 'U') -> Just NU
    ('N', 'Z') -> Just NZ
    ('O', 'M') -> Just OM
    ('P', 'A') -> Just PA
    ('P', 'E') -> Just PE
    ('P', 'F') -> Just PF
    ('P', 'G') -> Just PG
    ('P', 'H') -> Just PH
    ('P', 'K') -> Just PK
    ('P', 'L') -> Just PL
    ('P', 'M') -> Just PM
    ('P', 'N') -> Just PN
    ('P', 'R') -> Just PR
    ('P', 'S') -> Just PS
    ('P', 'T') -> Just PT
    ('P', 'W') -> Just PW
    ('P', 'Y') -> Just PY
    ('Q', 'A') -> Just QA
    ('R', 'E') -> Just RE
    ('R', 'O') -> Just RO
    ('R', 'S') -> Just RS
    ('R', 'U') -> Just RU
    ('R', 'W') -> Just RW
    ('S', 'A') -> Just SA
    ('S', 'B') -> Just SB
    ('S', 'C') -> Just SC
    ('S', 'D') -> Just SD
    ('S', 'E') -> Just SE
    ('S', 'G') -> Just SG
    ('S', 'H') -> Just SH
    ('S', 'I') -> Just SI
    ('S', 'J') -> Just SJ
    ('S', 'K') -> Just SK
    ('S', 'L') -> Just SL
    ('S', 'M') -> Just SM
    ('S', 'N') -> Just SN
    ('S', 'O') -> Just SO
    ('S', 'R') -> Just SR
    ('S', 'S') -> Just SS
    ('S', 'T') -> Just ST
    ('S', 'V') -> Just SV
    ('S', 'X') -> Just SX
    ('S', 'Y') -> Just SY
    ('S', 'Z') -> Just SZ
    ('T', 'C') -> Just TC
    ('T', 'D') -> Just TD
    ('T', 'F') -> Just TF
    ('T', 'G') -> Just TG
    ('T', 'H') -> Just TH
    ('T', 'J') -> Just TJ
    ('T', 'K') -> Just TK
    ('T', 'L') -> Just TL
    ('T', 'M') -> Just TM
    ('T', 'N') -> Just TN
    ('T', 'O') -> Just TO
    ('T', 'R') -> Just TR
    ('T', 'T') -> Just TT
    ('T', 'V') -> Just TV
    ('T', 'W') -> Just TW
    ('T', 'Z') -> Just TZ
    ('U', 'A') -> Just UA
    ('U', 'G') -> Just UG
    ('U', 'M') -> Just UM
    ('U', 'S') -> Just US
    ('U', 'Y') -> Just UY
    ('U', 'Z') -> Just UZ
    ('V', 'A') -> Just VA
    ('V', 'C') -> Just VC
    ('V', 'E') -> Just VE
    ('V', 'G') -> Just VG
    ('V', 'I') -> Just VI
    ('V', 'N') -> Just VN
    ('V', 'U') -> Just VU
    ('W', 'F') -> Just WF
    ('W', 'S') -> Just WS
    ('Y', 'E') -> Just YE
    ('Y', 'T') -> Just YT
    ('Z', 'A') -> Just ZA
    ('Z', 'M') -> Just ZM
    ('Z', 'W') -> Just ZW
    _ -> Nothing
{-# INLINE fromChars #-}

values :: IsString s => [(s, CountryCode)]
values = [(toString c, c) | c <- [minBound .. maxBound]]
{-# SPECIALISE values :: [(Text, CountryCode)] #-}
{-# INLINEABLE values #-}

decode :: Dhall.Decoder CountryCode
decode = Dhall.enum (Dhall.Map.fromList values)
{-# INLINEABLE decode #-}
