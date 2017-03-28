module Tp where 

data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float }deriving (Show,Eq)

mickeyMouse = CRaton 88 20 0.8
jerry = CRaton 76 2 0.3


estudioMasaCorporal :: Raton -> Float
estudioMasaCorporal raton = peso raton/(altura raton ^ 2)

estudioAntiguedad :: Raton -> Float
estudioAntiguedad raton = (edad raton + 5) / 85


analisisExceso :: (Raton -> Float) -> Float -> Raton -> Bool
analisisExceso estudio valor raton = estudio raton > valor

analisisRangoMedio :: (Raton -> Float) -> Raton -> Float -> Float -> Bool
analisisRangoMedio estudio raton valorMinimo valorMaximo = not(estudio raton > valorMinimo && estudio raton < valorMaximo)

analisisBerreta :: (Raton -> Float) -> Raton -> Bool
analisisBerreta _ _ = False


hierbaBuena :: Raton -> Raton
hierbaBuena (CRaton edad peso altura) = CRaton (rejuvenecerRaton edad) peso altura


hierbaMala :: Raton -> Raton
hierbaMala (CRaton edad peso altura) = CRaton (envejecerRaton edad) peso altura

alcachofa :: Float -> Raton -> Raton
alcachofa porcentaje (CRaton edad peso altura) = CRaton edad (reducirPeso porcentaje peso) altura

hierbaZort :: Raton -> Raton
hierbaZort _ = CRaton 0 0 0


rejuvenecerRaton :: Float -> Float
rejuvenecerRaton =(/2)

envejecerRaton :: Float -> Float
envejecerRaton = (*2)

reducirPeso:: Float -> Float -> Float
reducirPeso porcentaje peso = peso - ((porcentaje * peso)/100)


type Hierba = Raton -> Raton 
mezclarHierbas :: Hierba -> Hierba -> Hierba
mezclarHierbas = (.)

medicamento :: Raton -> [(Raton -> Raton)] -> Raton
medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> (Raton -> Raton) -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton (concat hierbas)

aplicarHastaQueDeFalse diagnostico raton hierba  
	| diagnostico raton = hierba raton
	| otherwise = raton


ratisalil = [hierbaZort, hierbaMala]
pondsAntiAge = [alcachofa 10 , hierbaBuena, hierbaBuena, hierbaBuena]

