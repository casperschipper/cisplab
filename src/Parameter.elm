module Parameter exposing (..)

type Parameter
    = Pitch
    | Velo
    | Duration
    | Channel


toString : Parameter -> String
toString p =
    case p of
        Pitch ->
            "pitch"

        Velo ->
            "velo"

        Duration ->
            "duration"

        Channel ->
            "channel"