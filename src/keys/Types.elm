module Keys.Types exposing (..)

{-| Represents phisical key on a keyboard
-}


type alias Key =
    { pressed : Bool
    , note : Note
    }


type alias Note =
    { letter : Letter
    , type_ : NoteType
    , octave : Octave
    }


type alias Frequency =
    Float


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


type NoteType
    = Natural
    | Sharp


type alias Octave =
    Int


type alias Model =
    { keys : List Key
    , octave : Octave
    }


type Msg
    = KeyPressed Key
    | KeyReleased Key
    | NoOp
