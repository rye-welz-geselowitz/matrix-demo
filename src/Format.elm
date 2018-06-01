module Format exposing (floatToString)


floatToString : Int -> Float -> String
floatToString decimalPlaces n =
    let
        beforeDecimal =
            truncate n |> toString

        scaleFactor =
            10 ^ decimalPlaces

        scaled =
            n * toFloat scaleFactor |> truncate

        afterDecimal =
            rem scaled scaleFactor |> abs |> toString
    in
    getSign n ++ beforeDecimal ++ "." ++ afterDecimal


getSign : Float -> String
getSign n =
    if n < 0 then
        "-"
    else
        ""
