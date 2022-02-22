module Cap5_testes where
import Capitulo4
import Cap4_aux
import Capitulo5
import Cap5_aux

--Definição do Menu como uma lista variacional
type Menu = VList Food

dessert = opt "Dessert" Cake
meat = "meat" <: vlist [Steak,Fries]
pasta = "pasta" <: Pasta `vcons` dessert
menu = alt "Main" [meat,pasta]

aperitif :: VList Food
aperitif = opt "Drink" Sherry