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
		it "mezclar una Hierba buena con una Hierba mala crea una hierba que no produce efecto" $do
			mezclarHierbas hierbaBuena hierbaMala jerry `shouldBe` jerry
		it "mezclar una Hierba buena y una alcachofa 0 crea una hierba que produce el mismo efecto que una Hierba buena" $do
			mezclarHierbas hierbaBuena (alcachofa 0) jerry `shouldBe` hierbaBuena jerry
		it "mezclar una Hierba mala  y una alcachofa 1 debe crear una hierba pero no producir el mismo efecto que una hierba buena" $do
			mezclarHierbas hierbaMala (alcachofa 1) jerry `shouldNotBe` hierbaMala jerry
		it  "el medicamento ratisalil combina una Hierba hierbaMala y una Hierba Zort que produce los mismos resultados que una Hierba Zort" $do
			medicamento jerry ratisalil `shouldBe` medicamento jerry [hierbaZort]
		it "el medicamento ponds anti age combina una alcachofa de 10 con 3 hierbas buenas que se le aplica a jerry quedando con edad = 9.5, peso = 1.8 y altura = 0.3" $do
			medicamento jerry pondsAntiAge `shouldBe` CRaton 9.5 1.8 0.3
		it "aplicarle un tratamiento contra la antiguedad a mickeyMouse con una hierbaBuena y una pondsAntiAge debe producir los mismos resultados que aplicarle una hierbaBuena" $do
			tratamiento (analisisExceso estudioAntiguedad 1) mickeyMouse [[hierbaBuena],pondsAntiAge] `shouldBe` hierbaBuena mickeyMouse 	
		it "aplicarle un tratamiento contra la antiguedad a jerry con una hierbaBuena y una pondsAntiAge debe producir los mismos resultados que aplicarle una hierbaBuena" $do
			tratamiento (analisisExceso estudioAntiguedad 1) jerry [[hierbaBuena],pondsAntiAge] `shouldBe` alcachofa 0 jerry




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

