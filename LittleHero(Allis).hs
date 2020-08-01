import Text.Show.Functions

type Equipamiento = (Personaje -> Personaje)

data Caracteristica = Caracteristica {ataque:: Float, defensa :: Int, agallas :: Int, agresividad :: Float} deriving (Show)

data Conocimiento = Conocimiento{temaQueSabe :: String,  area ::String} deriving (Show)
                
data Personaje =  Personaje { nombre :: String,  vida :: Int, caracteristicas :: Caracteristica, conocimientos :: [Conocimiento], equipamientos :: [Equipamiento]} deriving(Show)


--Analizando a los personajes:
--Se desea saber de un personaje:

--1.	Si está desmayado, que es cuando su vida está en 0.
desmayado :: Personaje -> Personaje
desmayado personaje = personaje { vida = 0} 

--2.	Si es Baymax (claramente, se sabe porque su nombre es Baymax).
baymax :: Personaje -> Personaje
baymax personaje = personaje{nombre = "baymax"}

--3.	Si es un cerebrito, si todos los conocimientos que tiene son de la misma área y al menos tiene 10 conocimientos. 
cerebrito :: String -> Personaje -> Bool
cerebrito unArea personaje =  todosLosConocimientosSonMismaArea unArea personaje &&  tineConocimiento personaje

todosLosConocimientosSonMismaArea unArea personaje =   all (conocimientoArea unArea ) (conocimientos personaje)

tineConocimiento =    ((>10).length.conocimientos) 


--4.	Si puede chamuyar de un área de conocimiento, esto es cuando tiene al menos un conocimiento del área. 
chamuyar :: String -> Personaje -> Bool
chamuyar  unArea personaje = any (conocimientoArea unArea) (conocimientos personaje)

conocimientoArea unArea = ((==unArea).area) 

-- desea saber de un personaje:

---5.	Su poder de ataque, es el ataque más un 10% de su agresividad. Si está desmayado no tiene poder de ataque.
poderAtaque :: Personaje -> Float
poderAtaque  personaje | not(estaDemayado personaje) = (ataque.caracteristicas) personaje + ((*0.1).agresividad.caracteristicas) personaje
                       | otherwise = 0

estaDemayado personaje = ((==0).ataque.caracteristicas) personaje


---6.	Su poder de defensa. Si está desmayado no tiene poder de defensa


poderDefensa personaje | not(estaDemayado personaje)  =  (defensa.caracteristicas)  personaje
                       | otherwise = 0 

--7.	Cuales pueden ser héroes, estos son los que sus agallas sean mayores a 100 o Baymax -Baymax no necesita agallas para ser un Héroe, 
--ya que es uno de los protagonistas principales de esta historia-.
cualesPuedenSerHerores :: [Personaje] -> [Personaje]
cualesPuedenSerHerores  =   filter (puedeSerHeroe) 

puedeSerHeroe personaje =   ((>100).agallas.caracteristicas) personaje || protagonistaPrincipal personaje

protagonistaPrincipal = ((=="Baymax -Baymax ").nombre) 

--8.	Cuales están listos para la batalla. Son aquellos que después de curarse están listos para pelear. 
--	Al curarse recuperan 10 puntos de vida.
--	Pueden pelear si su vida supera los 80 puntos.

listosParaBatalla personajes = filter despuesDeCurarse personajes

despuesDeCurarse  =   ((>80).vida.seCura) 

seCura personaje = personaje {vida = vida personaje + 10}



--9.	Agregar un equipamiento al personaje (se agrega para después usarlo). Ojo que a Baymax como tiene el chip de curar, 
--no se le pueden agregar elementos que aumenten su habilidad (Tip: simular que se le aplicó el equipamiento).


agregarEquipamiento unEquipo personaje = foldr ($)  personaje (equipamientosPer unEquipo personaje)

equipamientosPer unEquipo personaje = (equipamientos . agregar unEquipo) personaje

agregar unEquipo personaje | not (protagonistaPrincipal personaje) =  personaje {equipamientos = unEquipo : equipamientos  personaje} 
                           | otherwise = personaje 
--10.	Agregar una armadura a un personaje. Las armaduras son un equipamiento que puede otorgar varios efectos combinados, por ejemplo:
--	Iron: Aumenta el poder * 10, sube la defensa en 5, y baja la agresividad en 5.
--	Alma: aprender “Patada alta” del tema Kung Fu, incrementar la defensa en 80.
--Mostrar la invocación a agregarEquipamiento para que agregue las armaduras.


--agregarArmadura unArmadura personaje


--1.	Hacer que un personaje utilice todos los equipamientos que tienen. 
--Recordar que una vez puestos estos equipamientos, no podrá ponerselos de nuevo.
utilizarquipamiento personaje =   foldr ($) personaje (equipamientos personaje)

--12.	Encontrar los personajes más aptos para las siguientes misiones:
--	pelearConKabuki, luego de equiparse, todos los integrantes que estén listos para la batalla y además puedan ser héroes, 
--si no existiera ninguno siempre se puede recurrir a baymax.
--pelearConKabuki :: [Personaje] -> [Personaje] 
pelearConKabuki  unEquipo personajes =   (cualesPuedenSerHerores .listosParaBatalla .  map (agregarEquipamiento unEquipo)) personajes
--	convencerALaTia, luego de equiparse, todos los integrantes que puedanChamuyar sobre “Comida”, 
--si no existiera ninguno siempre se puede recurrir a baymax.
convencerALaTia unEquipo personajes =  ( filter puedanChamuyar . map (agregarEquipamiento unEquipo)) personajes

puedanChamuyar  personaje = all sabeElTema (conocimientos personaje)

sabeElTema =   ((=="comida").temaQueSabe)

--	irAJeopardy, luego de equiparse, todos los integrantes que son cerebritos, si no existiera ninguno siempre se puede recurrir a baymax.

--irAJeopardy  unTema personajes =  (filter cerebritos unTema .map (agregarEquipamiento unEquipo)) personajes


--13.
--Hay un equipamiento que se llama intersect, que básicamente lo que hace es enseñarle a una persona todos los niveles de Kung Fu:
-- KungFu nivel 1, KungFu nivel 2, KungFu nivel 3, etc. KungFu es el tema y el área es Artes Marciales
--Muestre como modelaria dicho equipamiento.

intersect = [Conocimiento "KungFuun 1" "Artes Marciales", Conocimiento "KungFuun 2" "Artes Marciales",Conocimiento "KungFuun 2" "Artes Marciales"]

