{-Programación Declarativa 2018-2-}

import Control.Parallel.Strategies
import System.Environment(getArgs)   

type ℝ = Double
data ℂ = Punto (ℝ,ℝ)

instance Num ℂ where
   (+) (Punto (a,b)) (Punto (c,d)) = error "Te toca"
   (*) (Punto (a,b)) (Punto (c,d)) = error "Te toca"


norma::ℂ->ℝ
norma (Punto (a,b)) = error "Te toca"


plano::ℝ->[ℂ]
plano n = error "Te toca"

                                         

mandelbrot_set::[ℂ]->String
mandelbrot_set = error "Te toca"

 
mandelbrot_set_parallel::[ℂ]->String
mandelbrot_set_parallel = error "Te toca"


main = error "Te toca"
