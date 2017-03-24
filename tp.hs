data Raton = CRaton {
	edad	 :: Float,
	peso	 :: Float,
	altura	 :: Float
}  deriving (Show)


estudioMasaCorporal raton = peso raton / (altura raton ^ 2)

estudioAntiguedad raton = (edad raton + 5) / 85

analisisDeExceso estudio raton valorCritico = estudio raton > valorCritico

analisisDeRangoMedio estudio raton valorMinimo valorMaximo = not (estudio raton > valorMinimo && estudio raton < valorMaximo)

analisisBerretas _ _ = False

