port module ImportExport exposing
  ( export
  , initImport
  , cancelImport
  , importData
  )

port export : String -> Cmd msg

port initImport : String -> Cmd msg

port cancelImport : String -> Cmd msg

port importData : (String -> msg) -> Sub msg
