module OneVoice exposing (..)
import Cisp exposing (CispProgram)

type alias OneVoice =
    {
        pitch : CispProgram
        ,velo : CispProgram
        ,channel : CispProgram
        ,duration : CispProgram
    }