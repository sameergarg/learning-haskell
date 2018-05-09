import Data.List

ifEvenInc x = if even x then x + 1 else x

ifEvenDouble x = if even x then 2*x else x

-- passing function as argument

ifEvenApplyFn fn x = if even x then fn x else x

inc x = x + 1

double x = 2*x

-- tuple

author = ("Will", "Kurt")

names = [("Will", "Kurt"),
         ("Russel", "Kurt"),
         ("Will", "Smith")]

compareLastNames n1 n2 = (\last1 last2 first1 first2 -> if last1 < last2
                         then LT
                         else if last1 > last2
                          then GT
                         else if first1 > first2
                          then GT
                          else if first1 < first2
                            then LT
                          else GT
                         ) (snd n1) (snd n2) (fst n1) (fst n2)

-- returning function

sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

getLocationFn location = case location of
  "ny"   -> nyOffice
  "sf"   -> sfOffice
  "reno" -> renoOffice
  _      -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = (\locationFn name -> locationFn name) (getLocationFn location) name

-- addressLetter ("Bob","Smith") "PO Box 1234 - San Francisco, CA, 94111"

