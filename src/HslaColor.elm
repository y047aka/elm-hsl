module HslaColor exposing
    ( HslaColor
    , hsl, hsla, hsl360, rgb255, rgb, rgba
    , fromRgba, fromHsla
    , toCssString
    , toRgba, toHsla, toColor
    , red, yellow, lime, cyan, blue, magenta
    , white, gray, black
    )

{-| This package defines a standard `Hsla` type.


# Types

@docs HslaColor


# Creating color


## From numbers

@docs hsl, hsla, hsl360, rgb255, rgb, rgba


## From records

@docs fromRgba, fromHsla


# Using colors with HTML/CSS/SVG

@docs toCssString


# Extracing values from `Hsla` type

@docs toRgba, toHsla, toColor


# Seed colors

@docs red, yellow, lime, cyan, blue, magenta


# white, gray, black

@docs white, gray, black

-}

import Color exposing (Color)


{-| Represents a color.
-}
type HslaColor
    = HslaSpace Float Float Float Float


{-| Creates a color from HSLA (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise and want full alpha, see [`hsl`](#hsl).

If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

-}
hsla : Float -> Float -> Float -> Float -> HslaColor
hsla h s l a =
    HslaSpace h s l a


{-| Creates a color from HSL (hue, saturation, lightness)
values between 0.0 and 1.0 (inclusive).

See also:

If you need to provide an alpha value, see [`hsla`](#hsla).

If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

-}
hsl : Float -> Float -> Float -> HslaColor
hsl h s l =
    HslaSpace h s l 1.0


{-| Creates a color from HSL (hue, saturation, lightness) float values between:

  - hue range is 0->360
  - saturation range is 0->100
  - lightness range is 0->100
  - alpha range is 0->1

See also:

If you want to provide Hsl values as `Float` values between 0.0 and 1.0, see [`hsl`](#hsl).

-}
hsl360 : Float -> Float -> Float -> HslaColor
hsl360 h s l =
    HslaSpace (h / 360) (s / 100) (l / 100) 1.0


{-| Creates a color from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise and want full alpha, see [`rgb`](#rgb).

If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

-}
rgba : Float -> Float -> Float -> Float -> HslaColor
rgba r g b a =
    let
        minColor =
            min r (min g b)

        maxColor =
            max r (max g b)

        h1 =
            if maxColor == r then
                (g - b) / (maxColor - minColor)

            else if maxColor == g then
                2 + (b - r) / (maxColor - minColor)

            else
                4 + (r - g) / (maxColor - minColor)

        h2 =
            h1 * (1 / 6)

        h3 =
            if isNaN h2 then
                0

            else if h2 < 0 then
                h2 + 1

            else
                h2

        l =
            (minColor + maxColor) / 2

        s =
            if minColor == maxColor then
                0

            else if l < 0.5 then
                (maxColor - minColor) / (maxColor + minColor)

            else
                (maxColor - minColor) / (2 - maxColor - minColor)
    in
    HslaSpace h3 s l a


{-| Creates a color from RGB (red, green, blue) values between 0.0 and 1.0 (inclusive).

This is a convenience function for making a color value with full opacity.

See also:

If you want to pass RGB values as `Int` values between 0 and 255, see [`rgb255`](#rgb255).

If you need to provide an alpha value, see [`rgba`](#rgba).

If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

-}
rgb : Float -> Float -> Float -> HslaColor
rgb r g b =
    rgba r g b 1.0


{-| Creates a color from RGB (red, green, blue) integer values between 0 and 255.

This is a convenience function if you find passing RGB channels as integers scaled to 255 more intuitive.

See also:

If you want to provide RGB values as `Float` values between 0.0 and 1.0, see [`rgb`](#rgb).

-}
rgb255 : Int -> Int -> Int -> HslaColor
rgb255 r g b =
    HslaSpace (scaleFrom255 r) (scaleFrom255 g) (scaleFrom255 b) 1.0


scaleFrom255 : Int -> Float
scaleFrom255 c =
    toFloat c / 255


{-| Creates a color from HSLA (hue, saturation, lightness, alpha) values between:

  - hue range is 0->360
  - saturation range is 0->100
  - lightness range is 0->100
  - alpha range is 0->1

See also:

If you want to be more concise, see [`hsla`](#hsla) or [`hsl`](#hsl).

-}
fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> HslaColor
fromHsla { hue, saturation, lightness, alpha } =
    HslaSpace hue saturation lightness alpha


{-| Creates from a record of RGBA values (red, green, blue, alpha) between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise, see [`rgba`](#rgba) or [`rgb`](#rgb).

-}
fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> HslaColor
fromRgba c =
    rgba c.red c.green c.blue c.alpha


{-| Converts a `Hsla` type to a string suitable for use in CSS.
The string will conform to [CSS Color Module Level 3](https://www.w3.org/TR/css-color-3/).

    Html.Attributes.style "background-color" (Hsl.toCssString Hsl.lightPurple)

-}
toCssString : HslaColor -> String
toCssString (HslaSpace h s l a) =
    let
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    String.concat
        [ "hsla("
        , String.fromFloat (roundTo h)
        , ","
        , String.fromFloat (pct s)
        , "%,"
        , String.fromFloat (pct l)
        , "%,"
        , String.fromFloat (roundTo a)
        , ")"
        ]


{-| Extract the HSLA (hue, saturation, lightness, alpha) components from a `Hsla` type.
The component values will be between 0.0 and 1.0 (inclusive).
-}
toHsla : HslaColor -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla (HslaSpace h s l a) =
    { hue = h, saturation = s, lightness = l, alpha = a }


{-| Extract the RGBA (red, green, blue, alpha) components `Hsla` type.
The component values will be between 0.0 and 1.0 (inclusive).
-}
toRgba : HslaColor -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba (HslaSpace hue sat light alpha) =
    let
        ( h, s, l ) =
            ( hue, sat, light )

        m2 =
            if l <= 0.5 then
                l * (s + 1)

            else
                l + s - l * s

        m1 =
            l * 2 - m2

        r =
            hueToRgb (h + 1 / 3)

        g =
            hueToRgb h

        b =
            hueToRgb (h - 1 / 3)

        hueToRgb h__ =
            let
                h_ =
                    if h__ < 0 then
                        h__ + 1

                    else if h__ > 1 then
                        h__ - 1

                    else
                        h__
            in
            if h_ * 6 < 1 then
                m1 + (m2 - m1) * h_ * 6

            else if h_ * 2 < 1 then
                m2

            else if h_ * 3 < 2 then
                m1 + (m2 - m1) * (2 / 3 - h_) * 6

            else
                m1
    in
    { red = r, green = g, blue = b, alpha = alpha }


{-| Convert an HslaColor to a Color
-}
toColor : HslaColor -> Color
toColor (HslaSpace h s l a) =
    Color.fromHsla
        { hue = h
        , saturation = s
        , lightness = l
        , alpha = a
        }


{-| -}
red : HslaColor
red =
    hsl360 0 100 50


{-| -}
yellow : HslaColor
yellow =
    hsl360 60 100 50


{-| -}
lime : HslaColor
lime =
    hsl360 120 100 50


{-| -}
cyan : HslaColor
cyan =
    hsl360 180 100 50


{-| -}
blue : HslaColor
blue =
    hsl360 240 100 50


{-| -}
magenta : HslaColor
magenta =
    hsl360 300 100 50


{-| -}
white : HslaColor
white =
    hsl360 0 0 100


{-| -}
gray : HslaColor
gray =
    hsl360 0 0 50


{-| -}
black : HslaColor
black =
    hsl360 0 0 0
