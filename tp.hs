data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float }deriving (Show)

mickeyMouse = CRaton 88 20 0.8
jerry = CRaton 76 2 0.3

estudioMasaCorporal raton = peso raton/(altura raton ^ 2)
estudioAntiguedad raton = (edad raton + 5) / 85

analisisExceso estudio raton valor = estudio raton > valor 	
analisisRangoMedio estudio raton valorMinimo valorMaximo = not(estudio raton > valorMinimo && estudio raton < valorMaximo)
analisisBerreta _ _ = False
	
hierbaBuena :: Raton -> Raton
hierbaBuena (CRaton edad peso altura) = CRaton (rejuvenecerRaton edad) peso altura

hierbaMala :: Raton -> Raton
hierbaMala (CRaton edad peso altura) = CRaton (envejecerRaton edad) peso altura

alcachofa :: Float -> Raton -> Raton
alcachofa alcachofa (CRaton edad peso altura) = CRaton edad (peso - reducirPeso alcachofa peso) altura

hierbaZort :: Raton -> Raton
hierbaZort _ = CRaton 0 0 0

rejuvenecerRaton =(/2)
envejecerRaton = (*2)
reducirPeso alcachofa peso = alcachofa / peso 

medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba raton hierba = hierba raton

ratisalil :: Raton -> Raton
ratisalil (CRaton edad peso altura) = hierbaMala(hierbaZort(CRaton edad peso altura))
