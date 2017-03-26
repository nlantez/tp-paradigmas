import Test.Hspec

runTests = hspec $do
	describe "Tests de diagnosticos:" $do
		it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Micky Mouse debería dar positivo" $do
			analisisExceso estudioAntiguedad 1 mickeyMouse `shouldBe` True 
		it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Jerry debería dar negativo" $do
			analisisExceso estudioAntiguedad 1 jerry `shouldBe` False 
		it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Micky Mouse deberia dar positivo" $do
			analisisRangoMedio estudioMasaCorporal mickeyMouse 18.5 25 `shouldBe` True
		it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Jerry deberia dar negativo" $do
			analisisRangoMedio estudioMasaCorporal jerry 18.5 25 `shouldBe` False
	describe "Tests medicinas y operaciones:" $do
		--it "mezclar una Hierba buena con una Hierba mala crea una hierba que no produce efecto"
		--it ""







data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float }deriving (Show)

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
--hierbaBuena (CRaton edad peso altura) = CRaton (rejuvenecerRaton edad) peso altura
hierbaBuena (CRaton edad peso altura) = CRaton (doble edad) peso altura


hierbaMala :: Raton -> Raton
--hierbaMala (CRaton edad peso altura) = CRaton (envejecerRaton edad) peso altura
hierbaMala (CRaton edad peso altura) = CRaton (mitad edad) peso altura

alcachofa :: Float -> Raton -> Raton
alcachofa porcentaje (CRaton edad peso altura) = CRaton edad (reducirPeso porcentaje peso) altura

hierbaZort :: Raton -> Raton
hierbaZort _ = CRaton 0 0 0

doble edad = edad * 2
mitad edad = edad / 2

--rejuvenecerRaton :: Float -> Float
--rejuvenecerRaton =(/2)

--envejecerRaton :: Float -> Float
--envejecerRaton = (*2)

reducirPeso:: Float -> Float -> Float
reducirPeso porcentaje peso = peso - ((porcentaje * peso)/100)


medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> (Raton -> Raton) -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton hierbas 

aplicarHastaQueDeFalse diagnostico raton hierba  
	| diagnostico raton = hierba raton
	| otherwise = raton


ratisalil :: Raton -> Raton
ratisalil raton = medicamento raton [hierbaZort,hierbaMala]

pondsAntiAge :: Raton -> Raton
pondsAntiAge raton = medicamento raton [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa 10]


