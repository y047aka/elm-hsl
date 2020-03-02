module HslaColor.Manipulate exposing (rotateHue, saturate, desaturate, lighten, darken, fadeIn, fadeOut)

{-| A library for creating and manipulating colors.


# Color adjustment

@docs rotateHue, saturate, desaturate, lighten, darken, fadeIn, fadeOut

-}

import HslaColor exposing (HslaColor, hsla, toHsla)


limit : Float -> Float
limit =
    clamp 0 1


{-| Change the hue of a color. The angle value must be in degrees
-}
rotateHue : Float -> HslaColor -> HslaColor
rotateHue angle cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsla cl
    in
    hsla (hue + degrees angle) saturation lightness alpha


{-| Increase the saturation of a color
-}
saturate : Float -> HslaColor -> HslaColor
saturate offset cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsla cl
    in
    hsla hue (limit (saturation + offset)) lightness alpha


{-| Decrease the saturation of a color
-}
desaturate : Float -> HslaColor -> HslaColor
desaturate offset cl =
    saturate -offset cl


{-| Increase the lightning of a color
-}
lighten : Float -> HslaColor -> HslaColor
lighten offset cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsla cl
    in
    hsla hue saturation (limit (lightness + offset)) alpha


{-| Decrease the lightning of a color
-}
darken : Float -> HslaColor -> HslaColor
darken offset cl =
    lighten -offset cl


{-| Increase the opacity of a color
-}
fadeIn : Float -> HslaColor -> HslaColor
fadeIn offset cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsla cl
    in
    hsla hue saturation lightness (limit (alpha + offset))


{-| Decrease the opacity of a color
-}
fadeOut : Float -> HslaColor -> HslaColor
fadeOut offset cl =
    fadeIn -offset cl
