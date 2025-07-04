module Main (main) where

main :: IO ()
main = print (calcMahjongPoint True 30 2)

calcMahjongPoint :: Bool -> Int -> Int -> Int -- UserType(oya or ko), Points(fu), Roles(yaku) -> GamePoints
calcMahjongPoint winnerTypes winnerPoints winnerRoles = 

  let
    mahjongType = winnerTypes :: Bool -- True is Oya, False is Ko
    mahjongRoles = winnerRoles :: Int -- Roles are the count of Yaku
    mahjongPoints = winnerPoints :: Int -- Points are the count of Fu

    mangan_Oya = 12000 :: Int
    haneman_Oya = 18000 :: Int
    baiman_Oya = 24000 :: Int 
    sanbaiman_Oya = 36000 :: Int
    yakuman_Oya = 48000 :: Int 

    mangan_Ko = 8000 :: Int
    haneman_Ko = 12000 :: Int
    baiman_Ko = 16000 :: Int
    sanbaiman_Ko = 24000 :: Int
    yakuman_Ko = 32000 :: Int
  in

    if mahjongType == True then -- Oya

        if mahjongPoints == 20 then

             case mahjongRoles of 
               1 -> 0
               2 -> 700
               3 -> 1300
               4 -> 2600
               _ -> 0
        
          else if mahjongPoints == 25 then

             case mahjongRoles of 
               1 -> 0
               2 -> 800
               3 -> 1600
               4 -> 3200
               _ -> 0
        
          else if mahjongPoints == 30 then

             case mahjongRoles of 
               1 -> 1500
               2 -> 2900
               3 -> 5800
               4 -> 11600
               _ -> 0
        
          else if mahjongPoints == 40 then

             case mahjongRoles of 
               1 -> 2000
               2 -> 3900
               3 -> 7700
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 50 then

             case mahjongRoles of 
               1 -> 2400
               2 -> 4800
               3 -> 9600
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 60 then

             case mahjongRoles of 
               1 -> 2900
               2 -> 5800
               3 -> 9600
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 70 then

             case mahjongRoles of 
               1 -> 3400
               2 -> 6800
               3 -> mangan_Oya
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 80 then

             case mahjongRoles of 
               1 -> 3900
               2 -> 7700
               3 -> mangan_Oya
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 90 then

             case mahjongRoles of 
               1 -> 4400
               2 -> 8700
               3 -> mangan_Oya
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 100 then

             case mahjongRoles of 
               1 -> 4800
               2 -> 9600
               3 -> mangan_Oya
               4 -> mangan_Oya
               _ -> 0
        
          else if mahjongPoints == 110 then 

             case mahjongRoles of 
               1 -> 5300
               2 -> 10600
               3 -> mangan_Oya
               4 -> mangan_Oya
               _ -> 0
          
          else 
            
            if mahjongRoles >= 13 then yakuman_Oya --Yakuman
               else if 10 < mahjongRoles && mahjongRoles < 13 then sanbaiman_Oya -- Sanbaiman
                 else if 7 < mahjongRoles && mahjongRoles < 11 then baiman_Oya -- Baiman
                   else if 5 < mahjongRoles && mahjongRoles < 8 then haneman_Oya -- Haneman
                     else mangan_Oya -- Mangan 
        
    else -- Ko
      
      if mahjongRoles >= 13 then yakuman_Ko --Yakuman
         else if 10 < mahjongRoles && mahjongRoles < 13 then sanbaiman_Ko -- Sanbaiman
           else if 7 < mahjongRoles && mahjongRoles < 11 then baiman_Ko -- Baiman
             else if 5 < mahjongRoles && mahjongRoles < 8 then haneman_Ko -- Haneman
      
      else mangan_Ko -- Mangan
      
      

        
    