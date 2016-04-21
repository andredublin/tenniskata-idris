module Tennis

%default total

data Player = PlayerOne | PlayerTwo

Eq Player where
  (==) PlayerOne PlayerOne = True
  (==) PlayerTwo PlayerTwo = True
  (==) _ _ = False

data Point = Love | Fifteen | Thirty

Eq Point where
  (==) Love Love = True
  (==) Fifteen Fifteen = True
  (==) Thirty Thirty = True
  (==) _ _ = False

record PointsData where
  constructor MkPointsData
  PlayerOnePoint, PlayerTwoPoint : Point

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

other : Player -> Player
other PlayerOne = PlayerTwo
other PlayerTwo = PlayerOne

incrementPoint : Point -> Maybe (Point)
incrementPoint Love = Just (Fifteen)
incrementPoint Fifteen = Just (Thirty)
incrementPoint Thirty = Nothing

pointTo : Player -> Point -> PointsData -> PointsData
pointTo PlayerOne point current = record { PlayerOnePoint = point } current
pointTo PlayerTwo point current = record { PlayerTwoPoint = point } current

pointFor : Player -> PointsData -> Point
pointFor PlayerOne current = PlayerOnePoint current
pointFor PlayerTwo current = PlayerTwoPoint current

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

-- State Machine
score : Score -> Player -> Score
score (Points p) winner = scoreWhenPoints p winner
score (Forty f) winner = scoreWhenForty f winner
score Deuce winner = scoreWhenDeuce winner
score (Advantage a) winner = scoreWhenAdvantage a winner
score (Game g) winner = scoreWhenGame g

newGame : Score
newGame = Points (MkPointsData Love Love)

-- Formatting
pointToString : Point -> String
pointToString Love = "Love"
pointToString Fifteen = "15"
pointToString Thirty = "30"

scoreToString : String -> String -> Score -> String
scoreToString playerOneName playerTwoName (Points p) =
  if PlayerOnePoint p == PlayerTwoPoint p then show (pointToString (PlayerOnePoint p) ++ "-All")
  else show (pointToString (PlayerOnePoint p) ++ "-" ++ pointToString (PlayerTwoPoint p))
scoreToString playerOneName playerTwoName (Forty f) =
  let other = pointToString (OtherPlayerPoint f) in
    if (Player' f) == PlayerOne then show ("40-" ++ other)
    else show (other ++ "-40")
scoreToString playerOneName playerTwoName Deuce = show "Deuce"
scoreToString playerOneName playerTwoName (Advantage a) =
  if a == PlayerOne then show ("Advantage " ++ playerOneName)
  else show ("Advantage " ++ playerTwoName)
scoreToString playerOneName playerTwoName (Game g) =
  if g == PlayerOne then show ("Game " ++ playerOneName)
  else show ("Game " ++ playerTwoName)

-- The Game
main : IO ()
main = do
  let playSequence = [PlayerTwo, PlayerTwo, PlayerOne, PlayerTwo, PlayerOne, PlayerOne, PlayerTwo, PlayerTwo]
  let final = foldl score newGame playSequence
  putStrLn (scoreToString "Björn Borg" "John McEnroe" final)
