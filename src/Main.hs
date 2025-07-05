module Main (main) where

data MahjongType = Oya | Ko deriving Eq

data ScoreRank = Mangan | Haneman | Baiman | Sanbaiman | Yakuman deriving Eq

type MahjongPoint = Int

type MahjongRole = Int

scoreRank :: MahjongType -> ScoreRank -> Int
scoreRank Oya Mangan    = 12000
scoreRank Oya Haneman   = 18000
scoreRank Oya Baiman    = 24000
scoreRank Oya Sanbaiman = 36000
scoreRank Oya Yakuman   = 48000

scoreRank Ko Mangan     = 8000
scoreRank Ko Haneman    = 12000
scoreRank Ko Baiman     = 16000
scoreRank Ko Sanbaiman  = 24000
scoreRank Ko Yakuman    = 32000

main :: IO ()
main = print (calcMahjongGamePoint Oya 30 13)

calcMahjongGamePoint :: MahjongType -> MahjongPoint -> MahjongRole -> Int

calcMahjongGamePoint mahjongType _ mahjongRoles | mahjongRoles >= 5 =
  scoreRank mahjongType calculateScoreRank
  where
    calculateScoreRank
      | mahjongRoles >= 13                     = Yakuman
      | 10 < mahjongRoles && mahjongRoles < 13 = Sanbaiman
      | 7  < mahjongRoles && mahjongRoles < 11 = Baiman
      | 5  < mahjongRoles && mahjongRoles < 8  = Haneman
      | otherwise                              = Mangan

calcMahjongGamePoint Oya mahjongPoints mahjongRoles =
  case mahjongPoints of
    20 ->
      case mahjongRoles of
        1 -> error "impossible"
        2 -> 2100
        3 -> 3900
        4 -> 7800
        _ -> error "impossible"

    25 ->
      case mahjongRoles of
        1 -> error "impossible"
        2 -> 2400
        3 -> 4800
        4 -> 9600
        _ -> error "impossible"

    30 ->
      case mahjongRoles of
        1 -> 1500
        2 -> 2900
        3 -> 5800
        4 -> 11600
        _ -> error "impossible"

    40 ->
      case mahjongRoles of
        1 -> 2000
        2 -> 3900
        3 -> 7700
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    50 ->
      case mahjongRoles of
        1 -> 2400
        2 -> 4800
        3 -> 9600
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    60 ->
      case mahjongRoles of
        1 -> 2900
        2 -> 5800
        3 -> 11600
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    70 ->
      case mahjongRoles of
        1 -> 3400
        2 -> 6800
        3 -> scoreRank Oya Mangan
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    80 ->
      case mahjongRoles of
        1 -> 3900
        2 -> 7700
        3 -> scoreRank Oya Mangan
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    90 ->
      case mahjongRoles of
        1 -> 4400
        2 -> 8700
        3 -> scoreRank Oya Mangan
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    100 ->
      case mahjongRoles of
        1 -> 4800
        2 -> 9600
        3 -> scoreRank Oya Mangan
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    110 ->
      case mahjongRoles of
        1 -> 5300
        2 -> 10600
        3 -> scoreRank Oya Mangan
        4 -> scoreRank Oya Mangan
        _ -> error "impossible"

    x ->
      error (show x <> " is not a valid MahjongPoint!")

calcMahjongGamePoint Ko mahjongPoints mahjongRoles =
  case mahjongPoints of
    20 ->
      case mahjongRoles of
        1 -> error "impossible"
        2 -> 1500
        3 -> 2700
        4 -> 5200
        _ -> error "impossible"

    25 ->
      case mahjongRoles of
        1 -> error "impossible"
        2 -> 1600
        3 -> 3200
        4 -> 6400
        _ -> error "impossible"

    30 ->
      case mahjongRoles of
        1 -> 1000
        2 -> 2000
        3 -> 3900
        4 -> 7700
        _ -> error "impossible"

    40 ->
      case mahjongRoles of
        1 -> 1300
        2 -> 2600
        3 -> 5200
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    50 ->
      case mahjongRoles of
        1 -> 1600
        2 -> 3200
        3 -> 6400
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    60 ->
      case mahjongRoles of
        1 -> 2000
        2 -> 3900
        3 -> 7700
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    70 ->
      case mahjongRoles of
        1 -> 2300
        2 -> 4500
        3 -> scoreRank Ko Mangan
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    80 ->
      case mahjongRoles of
        1 -> 2600
        2 -> 5200
        3 -> scoreRank Ko Mangan
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    90 ->
      case mahjongRoles of
        1 -> 2900
        2 -> 5800
        3 -> scoreRank Ko Mangan
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    100 ->
      case mahjongRoles of
        1 -> 3200
        2 -> 6400
        3 -> scoreRank Ko Mangan
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    110 ->
      case mahjongRoles of
        1 -> 3600
        2 -> 7100
        3 -> scoreRank Ko Mangan
        4 -> scoreRank Ko Mangan
        _ -> error "impossible"

    x ->
      error (show x <> " is not a valid MahjongPoint!")