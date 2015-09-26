
squareM :: _ -> _
squareM = #x -> `(,x * ,x)   -- `#` indicates macro-lambda


macro_expand {squareM 3}
