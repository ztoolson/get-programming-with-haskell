import Data.Function (on)

-- Defining the patientInfo function
patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

updatedPatientInfo :: FirstName -> LastName -> Age -> Height -> String
updatedPatientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- Could be more sensible to store patient names in a tuple together
type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

testPatient :: PatientName
testPatient = ("John", "Doe")

-- quick check 12.1: Rewrite patientInfo to use your patient Name type
patInfo :: PatientName -> Age -> Height -> String
patInfo (fname, lname) age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- Creating New Types

data Sex = Male | Female -- Sex is the type constructor. Sex type is an instance of either of these data constructors (Male | Female). The data constructor can be used just like values.

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- Create a type for blood types
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- Write a function if one patient can donate blood to another
-- Rules (ignore Rh compatibililty for this example)
-- A can donate to A and AB
-- B can donate to B and AB
-- AB can donante only to AB
-- O can donate to anybody

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- Universal Donor
canDonateTo _ (BloodType AB _) = True -- Universal Receiver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

-- Expand the Name type to include a middle name
-- defined above
-- type FirstName = String
-- type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

-- Using record syntax

-- Name Sex Age Height Weight BloodTyoe
data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- quick check 12.2 Create Jane Elizabeth Smith patient
janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 30 65 130 (BloodType AB Pos)

-- Patient with record syntax
data Patient' = Patient' {
      name :: Name
    , sex :: Sex
    , age :: Int
    , height :: Int
    , weight :: Int
    , bloodType :: BloodType
}

jackieSmith :: Patient'
jackieSmith = Patient' {
      name = Name "Jackie" "Smith"
    , age = 43
    , sex = Female
    , height = 62
    , weight = 115
    , bloodType = BloodType O Neg
}

jackieSmithUpdated = jackieSmith { age = 44 }

-- Q12.1: Write a function similar to canDonateTo that takes two patients as arguements rather than 2 bloodtypes
patientCanDonateTo :: Patient' -> Patient' -> Bool
patientCanDonateTo donor recipient = canDonateTo (bloodType donor) (bloodType recipient)
-- patientCanDonateTo  = canDonateTo `on` bloodType

-- Q12.2 Implement a patientSummary function taht uses your final Patient type. patientSummary should output a string that looks like this:
--  **************
--  Patient Name: Smith, John
--  Sex: Male
--  Age: 46
--  Height: 72 in.
--  Weight: 210 lbs.
--  Blood Type: AB+
--  **************

patientSummary :: Patient' -> String
patientSummary patient = 
    "**************\n" ++
    "Patient Name: " ++ showName (name patient) ++ "\n" ++
    "Sex: " ++ showSex (sex patient) ++ "\n" ++
    "Age: " ++ show (age patient) ++ "\n" ++
    "Height: " ++ show (height patient) ++ " in.\n" ++
    "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
    "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
    "**************"

-- Helper functions to show data in desired format
showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"


showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh