data Raton = CRaton {
	edad :: Float, 
	peso :: Float, 
	altura :: Float
 }deriving (Show)

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

alcachofa :: Raton -> Float -> Raton
alcachofa (CRaton edad peso altura) alcachofa = CRaton edad (reducirPeso (alcachofa peso)) altura

hierbaZort :: Raton -> Raton
hierbaZort _ = CRaton 0 0 0

rejuvenecerRaton = ( / )  2
envejecerRaton = ( * ) 2
reducirPeso porcentaje peso = peso - (alcachofa / peso) 

-- no podria ser:
-- mezclarHierbas hierba1 hierba2 raton = segundaHierba (primeraHierba raton) ?????
mezclarHierbas primerHierba segundaHierba (CRaton edad peso altura) = segundaHierba(primerHierba(CRaton edad peso altura))

tomarMedicamento listaDeHierbas raton = map raton listaDeHierbas

ratisalil :: Raton -> Raton
ratisalil (CRaton edad peso altura) = hierbaMala(hierbaZort(CRaton edad peso altura))
