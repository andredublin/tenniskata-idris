module Tennis

data Player = PlayerOne | PlayerTwo

Eq Player where
  (==) PlayerOne PlayerOne = True
  (==) PlayerTwo PlayerTwo = True
  (==) _ _ = False

-- PlayerType is a type level function
PlayerType : Player -> Type
PlayerType PlayerOne = Player
PlayerType PlayerTwo = Player

data Point = Love | Fifteen | Thirty

Eq Point where
  (==) Love Love = True
  (==) Fifteen Fifteen = True
  (==) Thirty Thirty = True
  (==) _ _ = False

-- PointType is a type level function
PointType : Point -> Type
PointType Love = Point
PointType Fifteen = Point
PointType Thirty = Point

record PointsData where
  constructor MkPointsData
  PlayerOnePoint, PlayerTwoPoint : Point

playerOnePoint : PointsData -> Point
playerOnePoint (MkPointsData PlayerOnePoint' PlayerTwoPoint') = PlayerOnePoint'

playerTwoPoint : PointsData -> Point
playerTwoPoint (MkPointsData PlayerOnePoint' PlayerTwoPoint') = PlayerTwoPoint'

record FortyData where
  constructor MkFortyData
  Player' : Player
  OtherPlayerPoint : Point

data Score =
  Points PointsData
  | Forty FortyData
  | Deuce
  | Advantage Player
  | Game Player

-- PlayerType player as the return type is an example of using dependent pattern matching
other : (player : Player) -> PlayerType player
other PlayerOne = PlayerTwo
other PlayerTwo = PlayerOne

incrementPoint : (point : Point) -> Maybe (PointType point)
incrementPoint Love = Just (Fifteen)
incrementPoint Fifteen = Just (Thirty)
incrementPoint Thirty = Nothing

pointTo : Player -> Point -> PointsData -> PointsData
pointTo PlayerOne point current = record { PlayerOnePoint = point } current
pointTo PlayerTwo point current = record { PlayerTwoPoint = point } current

pointFor : PlayerType player -> PointsData -> Point
pointFor {player = PlayerOne} _ current = playerOnePoint current
pointFor {player = PlayerTwo} _ current = playerTwoPoint current

scoreWhenGame : Player -> Score
scoreWhenGame winner = Game winner

scoreWhenAdvantage : Player -> Player -> Score
scoreWhenAdvantage advantagePlayer winner =
  if advantagePlayer == winner then Game winner
  else Deuce

scoreWhenDeuce : Player -> Score
scoreWhenDeuce winner = Advantage winner

scoreWhenForty : FortyData -> Player -> Score
scoreWhenForty current winner =
  if winner == Player' current then Game winner
  else Deuce

scoreWhenPoints : PointsData -> Player -> Score
scoreWhenPoints current winner =
  let point = incrementPoint (pointFor winner current) in
    case point of
      Nothing => Forty (MkFortyData winner (pointFor (other winner) current))
      (Just np) => Points (pointTo winner np current)
