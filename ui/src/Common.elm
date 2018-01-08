module Common exposing
  ( Dimension
  , Row
  , Column
  , Coordinate
  , Level
  , Size
  )


-- TYPES

type alias Dimension = 
  { width : Int
  , height : Int
  }

type alias Row = Int
type alias Column = Int
type alias Coordinate = (Row, Column)

type alias Level = Int

type alias Size =
  { width : Int
  , height : Int
  }

 