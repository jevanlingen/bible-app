module Types exposing (Msg(..), Call(..))

import Http

type Msg
  = ChangeBook String
  | ChangeChapter String
  | ScriptureChanged (Result Http.Error String)

type Call
  = Loading
  | Success String
  | Failure