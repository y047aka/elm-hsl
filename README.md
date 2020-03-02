# elm-hsl-color

This package is the HSL version of [elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest/)

## Example

```elm
import Hsl exposing (HslaColor)
import Html exposing (Html)
import Html.Attributes exposing (style)

view : HslaColor -> Html msg
view foreground =
    let
        hue =
            (Hsl.toHsla foreground).hue

        borderColor =
            Hsl.hsla hue 0.75 0.5 0.8
    in
    Html.div
        [ style "background-color" (Hsl.toCssString Hsl.lightOrange)
        , style "color" (Hsl.toCssString foreground)
        , style "border-color" (Hsl.toCssString borderColor)
        ]
        [ Html.text "(ᵔᴥᵔ)" ]
```
