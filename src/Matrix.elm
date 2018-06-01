module Matrix exposing (Matrix, empty, fromList, multiply, toList, transpose)

import Array exposing (Array)


type Matrix a
    = Matrix
        { dimensions : ( Int, Int ) -- (m row x n cols)
        , data : Array a
        }



--REPRESENTATION CONVERSIONS


{-| Takes a list of lists. Each member list represents a row in the matrix.
Returns either
(a) Just a Matrix representation of the data or
(b) Nothing, if the number of columns, in the list of rows is inconsistent
-}
fromList : List (List a) -> Maybe (Matrix a)
fromList vals =
    getNumCols vals
        |> Maybe.map
            (empty
                |> setData (vals |> List.concatMap identity |> Array.fromList)
                |> setRowNumber (List.length vals)
                |> flip setColNumber
            )


{-| Produces a list representation of a matrix, with each member list
representing a row in the matrix
-}
toList : Matrix a -> List (List a)
toList matrix =
    data matrix
        |> Array.toList
        |> List.indexedMap (,)
        |> List.foldl (buildListFromMatrix matrix) []



--MATRIX OPERATIONS


{-| Multiplies two matrices a and b. If the dimensions allow for multiplcation, returns
Just ab. Otherwise returns Nothing
-}
multiply : Matrix Float -> Matrix Float -> Maybe (Matrix Float)
multiply a b =
    let
        ( m, n ) =
            ( getM a, getN b )
    in
    if getN a == getM b && (m > 0) && (n > 0) then
        Array.initialize (m * n) identity
            |> Array.map (idxToIj ( m, n ) >> dotProductAtCoordinates a b)
            |> matrix ( m, n )
            |> Just
    else
        Nothing


{-| Transposes a matrix
-}
transpose : Matrix a -> Matrix a
transpose a =
    let
        ( m, n ) =
            a |> dimensions |> flipTuple
    in
    Array.initialize (m * n) identity
        |> Array.foldl (accumulateTransposedData a) Array.empty
        |> matrix ( m, n )



--HELPERS


dotProductAtCoordinates : Matrix Float -> Matrix Float -> ( Int, Int ) -> Float
dotProductAtCoordinates a b ( i, j ) =
    List.range 0 (getN a - 1)
        |> List.foldl
            (\idx2 acc ->
                case ( get a ( i, idx2 ), get b ( idx2, j ) ) of
                    ( Just itemFromA, Just itemFromB ) ->
                        acc + (itemFromA * itemFromB)

                    _ ->
                        acc
            )
            0.0


accumulateTransposedData : Matrix a -> Int -> Array a -> Array a
accumulateTransposedData a idx acc =
    case idx |> idxToIj ( getN a, getM a ) |> flipTuple |> get a of
        Nothing ->
            acc

        Just item ->
            Array.push item acc


buildListFromMatrix : Matrix a -> ( Int, a ) -> List (List a) -> List (List a)
buildListFromMatrix matrix ( idx, val ) acc =
    let
        ( i, j ) =
            idxToIj (dimensions matrix) idx
    in
    case List.reverse acc of
        lastRow :: rest ->
            if i >= List.length acc then
                initiateRow acc val
            else
                List.append lastRow [ val ] :: rest |> List.reverse

        _ ->
            initiateRow acc val


empty : Matrix a
empty =
    matrix ( 0, 0 ) Array.empty


getIdx : ( Int, Int ) -> Matrix a -> Int
getIdx ( i, j ) matrix =
    (i * getN matrix) + j


get : Matrix a -> ( Int, Int ) -> Maybe a
get matrix ( i, j ) =
    let
        idx =
            getIdx ( i, j ) matrix
    in
    matrix |> data |> Array.get idx


idxToIj : ( Int, Int ) -> Int -> ( Int, Int )
idxToIj ( _, colCount ) idx =
    ( idx // colCount, rem idx colCount )


flipTuple : ( a, a ) -> ( a, a )
flipTuple ( a, b ) =
    ( b, a )


matrix : ( Int, Int ) -> Array a -> Matrix a
matrix dimensions data =
    Matrix
        { dimensions = dimensions
        , data = data
        }


setData : Array a -> Matrix a -> Matrix a
setData data (Matrix matrix) =
    Matrix { matrix | data = data }


setRowNumber : Int -> Matrix a -> Matrix a
setRowNumber m (Matrix matrix) =
    Matrix { matrix | dimensions = ( m, Tuple.second matrix.dimensions ) }


setColNumber : Int -> Matrix a -> Matrix a
setColNumber n (Matrix matrix) =
    Matrix { matrix | dimensions = ( Tuple.first matrix.dimensions, n ) }


setDimensions : ( Int, Int ) -> Matrix a -> Matrix a
setDimensions dimensions (Matrix matrix) =
    Matrix { matrix | dimensions = dimensions }


dimensions : Matrix a -> ( Int, Int )
dimensions (Matrix { dimensions, data }) =
    dimensions


data : Matrix a -> Array a
data (Matrix { dimensions, data }) =
    data


getN : Matrix a -> Int
getN =
    dimensions >> Tuple.second


getM : Matrix a -> Int
getM =
    dimensions >> Tuple.first


getNumCols : List (List a) -> Maybe Int
getNumCols matrix =
    let
        n =
            List.head matrix |> Maybe.withDefault [] |> List.length
    in
    if matrix |> validateList (validateNumColsForRow n) then
        Just n
    else
        Nothing


validateList : (a -> Bool) -> List a -> Bool
validateList f list =
    list |> List.foldl (accumulateValidation f) True


accumulateValidation : (a -> Bool) -> a -> Bool -> Bool
accumulateValidation f item acc =
    if not acc then
        acc
    else
        f item


validateNumColsForRow : Int -> List a -> Bool
validateNumColsForRow n row =
    List.length row == n


initiateRow : List (List a) -> a -> List (List a)
initiateRow list item =
    List.append list [ [ item ] ]
