{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
----------------------------------------------------------------------------
module TypeLevel.JSON where
-----------------------------------------------------------------------------
-- |
-- Module      :  TypeLevel.JSON
-- Copyright   :  (C) 2022-2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  code@dmj.io
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
import GHC.TypeLits
----------------------------------------------------------------------------
data Value
  = Object [(Symbol, Value)]
  | Array [Value]
  | JString Symbol
  | Number Nat
  | Null
  | Bool Bool
----------------------------------------------------------------------------
data Token
  = WhiteSpace
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | TChar Char
  | Comma
  | Colon
  | Tick
  | Period
  | Plus
  | Minus
  | Quotes
  | TNum Nat
----------------------------------------------------------------------------
data Exp = E
----------------------------------------------------------------------------
data JDouble = JDouble Sign Nat Nat Exp Sign Nat

data Sign
  = Positive
  | Negative

type family ParseArray (tokens :: [Token]) :: ([Token], Value) where
  ParseArray
    (LBracket
      ': RBracket
      ': xs) = '( xs, Array '[] )
  ParseArray (LBracket ': tokens) =
     AccumValues tokens

type family AccumValues tokens where
  AccumValues tokens = ParseValuesByComma' '(ParseValue tokens, '[])

type family ParseValuesByComma' vals where
  ParseValuesByComma' '( '( 'Comma ': tokens, value ), arrayVals )
    = ParseValuesByComma'
    '( ParseValue tokens
     , arrayVals ++ '[value]
     )
  ParseValuesByComma' '( '(RBracket ': tokens, value), arrayVals)
    = '( tokens
       , 'Array (arrayVals ++ '[value])
       )

type family Decode a where
  Decode x = Snd (ParseValue (Lexer x))

type family ParseValue (tokens :: [Token]) :: ([Token], Value) where
  ParseValue
    (  TChar 'n'
    ': TChar 'u'
    ': TChar 'l'
    ': TChar 'l'
    ': tokens
    ) = '( tokens, 'Null )
  ParseValue
    (  TChar 't'
    ': TChar 'r'
    ': TChar 'u'
    ': TChar 'e'
    ': tokens
    ) = '( tokens, 'Bool True )
  ParseValue
    (  TChar 'f'
    ': TChar 'a'
    ': TChar 'l'
    ': TChar 's'
    ': TChar 'e'
    ': tokens
    ) = '( tokens, 'Bool False )

  ParseValue (LBracket ': tokens ) = ParseArray (LBracket ': tokens)
  ParseValue (LBrace ': tokens )   = ParseObject (LBrace ': tokens)
  ParseValue (Quotes ': tokens )   = ParseString (Quotes ': tokens)

type family ParseObject (tokens :: [Token]) :: ([Token], Value) where
  ParseObject (LBrace ': RBrace ': xs) = '( xs, Object '[] )
  ParseObject (LBrace ': tokens) = AccumKeyValues tokens

type family AccumKeyValues tokens where
  AccumKeyValues tokens = ParseKeyValuesByColon' '(ParseString tokens, '[])

type family ParseKeyValuesByColon' vals where
  ParseKeyValuesByColon' '( '( 'Colon ': tokens, JString key ), keyValuePairs )
    = ParseKeyValueIntoObject
    '( ParseValue tokens
     , key
     , keyValuePairs
     )

type family ParseKeyValueIntoObject vals where
  ParseKeyValueIntoObject '( '( RBrace ': tokens, value), key, keyValuePairs) =
    '(tokens, 'Object ( keyValuePairs ++ '[ '(key, value) ]))
  ParseKeyValueIntoObject '( '( Comma ': tokens, value), key, keyValuePairs) =
    ParseKeyValuesByColon' '(ParseString tokens, keyValuePairs ++ '[ '(key, value) ])

type family ParseString xs :: ([Token], Value) where
  ParseString (Quotes ': tokens) = ParseString' '(tokens, "")

type family ParseString' xs :: ([Token], Value) where
  ParseString' '( Quotes ': tokens, str) = '(tokens, JString str)
  ParseString' '( TChar c ': tokens, str) = ParseString' '(tokens, c `SnocSymbol` str)

type family SnocSymbol (c :: Char) (sym :: Symbol) where
  SnocSymbol c sym = sym `AppendSymbol` (c `ConsSymbol` "")

type family ParseNat tokens :: ([Token], Nat, [Nat]) where
  ParseNat tokens = ParseNat' '(tokens, 0, '[])

type family ParseNat' val :: ([Token], Nat, [Nat]) where
  ParseNat' '( '[], acc, nums) =
    '( '[], acc, nums)
  ParseNat' '( 'TNum num ': tokens, acc, nums ) =
    ParseNat' '(tokens, acc + 1, ((10 ^ acc) * num) ': nums)
  ParseNat' '(tokens, acc, nums ) =
    '(tokens, acc, nums)

type family Sum (n :: [Nat]) where
  Sum '[] = 0
  Sum (x ': xs) = x + Sum xs

type family Reverse (n :: [k]) :: [k] where
  Reverse '[] = '[]
  Reverse (x ': xs) = Reverse xs ++ '[x]

type family SmashWhite (xs :: [Token]) where
  SmashWhite '[] = '[]
  SmashWhite (WhiteSpace ': xs) = SmashWhite xs
  SmashWhite (x ': xs) = x ': SmashWhite xs

-- | Post-processing entails removing white space and
type family Lexer (xs :: Symbol) where
  Lexer xs = SmashWhite (Lex xs)

type family Lex (xs :: Symbol) where
  Lex xs = Lex' (UnconsSymbol xs)

type family Lex' (xs :: Maybe (Char,Symbol)) where
  Lex' 'Nothing             = '[]
  Lex' ('Just '( '{', xs))  = LBrace     ': Lex xs
  Lex' ('Just '( '}', xs))  = RBrace     ': Lex xs
  Lex' ('Just '( '[', xs))  = LBracket   ': Lex xs
  Lex' ('Just '( ']', xs))  = RBracket   ': Lex xs
  Lex' ('Just '( ',', xs))  = Comma      ': Lex xs
  Lex' ('Just '( ':', xs))  = Colon      ': Lex xs
  Lex' ('Just '( '\"', xs)) = Quotes     ': LexString xs -- ^ layered-lexer for Symbol accumulation
  Lex' ('Just '( '\'', xs)) = Tick       ': Lex xs
  Lex' ('Just '( '.', xs))  = Period     ': Lex xs
  Lex' ('Just '( '+', xs))  = Plus       ': Lex xs
  Lex' ('Just '( '-', xs))  = Plus       ': Lex xs
  Lex' ('Just '( '0', xs))  = TNum 0     ': Lex xs
  Lex' ('Just '( '1', xs))  = TNum 1     ': Lex xs
  Lex' ('Just '( '2', xs))  = TNum 2     ': Lex xs
  Lex' ('Just '( '3', xs))  = TNum 3     ': Lex xs
  Lex' ('Just '( '4', xs))  = TNum 4     ': Lex xs
  Lex' ('Just '( '5', xs))  = TNum 5     ': Lex xs
  Lex' ('Just '( '6', xs))  = TNum 6     ': Lex xs
  Lex' ('Just '( '7', xs))  = TNum 7     ': Lex xs
  Lex' ('Just '( '8', xs))  = TNum 8     ': Lex xs
  Lex' ('Just '( '9', xs))  = TNum 9     ': Lex xs
  Lex' ('Just '( ' ', xs))  = WhiteSpace ': Lex xs
  Lex' ('Just '( '\r', xs)) = WhiteSpace ': Lex xs
  Lex' ('Just '( '\t', xs)) = WhiteSpace ': Lex xs
  Lex' ('Just '( '\n', xs)) = WhiteSpace ': Lex xs
  Lex' ('Just '( c, xs))    = TChar c    ': Lex xs

type family LexString xs where
  LexString xs = LexString' (UnconsSymbol xs)

type family LexString' xs where
  LexString' Nothing = '[] -- todo error
    -- TypeError (Text "Lexical error, unterminated string quotation")
  LexString' (Just '( '\"', xs)) = Quotes ': Lex xs
  LexString' (Just '(c,xs)) = TChar c ': LexString xs

-- | Utils
type family a ++ b where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

type family Fst a where
  Fst '(a,b) = a

type family Snd a where
  Snd '(a,b) = b

