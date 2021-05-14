import Text.Show.Functions


type Tactica = String
type Acciones= Jugador->Jugador

data Propiedad = UnaPropiedad{
	nombreDePropiedad            :: String,
	precioDePropiedad            :: Int
}deriving (Show)

data Jugador = UnJugador {
	nombre                       :: String,
	cantidadDeDinero             :: Int	,
	tacticaDeJuego               :: Tactica,
	propiedadesAdquiridas        :: [Propiedad],
	accionesRealizadas           :: [Acciones]
}deriving (Show)

--Jugadores
carolina :: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [baltica, nuevaYork] [pasarPorElBanco]

--Propiedades
nuevaYork::Propiedad
nuevaYork= UnaPropiedad "Avenida Nueva York" 200

baltica::Propiedad
baltica = UnaPropiedad  "Avenida Baltica"    60 

virginia::Propiedad
virginia= UnaPropiedad "Avenida Virginia"    120

pasarPorElBanco:: Acciones
pasarPorElBanco = cambioDeTactica ("Comprador Compulsivo"). dinero (+40)  

dinero::(Int->Int) -> Jugador -> Jugador
dinero perdidoOGanado algunJugador = algunJugador {cantidadDeDinero= max 0.perdidoOGanado $cantidadDeDinero algunJugador}

cambioDeTactica:: Tactica -> Jugador -> Jugador
cambioDeTactica tacticaNueva algunJugador = algunJugador {tacticaDeJuego = tacticaNueva}

enojarse:: Acciones
enojarse =  dinero (+50) . agregarAcciones gritar


accionRealizada:: ([Acciones]->[Acciones]) -> Jugador -> Jugador
accionRealizada acciones algunJugador = algunJugador {accionesRealizadas = acciones $ accionesRealizadas algunJugador} 

agregarAcciones:: Acciones->Jugador->Jugador
agregarAcciones accion= accionRealizada (accion:)

gritar:: Acciones
gritar algunJugador = algunJugador {nombre = "AHHHH" ++ nombre algunJugador}

subastar:: Propiedad->Acciones
subastar algunaPropiedad algunJugador
    |  tieneLaTacticaAdecuada algunJugador = adquirirPropiedad algunaPropiedad (dinero (subtract (precioDePropiedad algunaPropiedad)) algunJugador
    |  otherwise = algunJugador

tieneLaTacticaAdecuada:: Jugador->Bool
tieneLaTacticaAdecuada algunJugador= esAccionista algunJugador || esOferenteSingular algunJugador

esAccionista:: Jugador->Bool
esAccionista = (=="Accionista").tacticaDeJuego

esOferenteSingular:: Jugador->Bool
esOferenteSingular = (=="Oferente Singular").tacticaDeJuego  

propiedadComprada :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador 
propiedadComprada compra algunJugador = algunJugador { propiedadesAdquiridas = compra $ propiedadesAdquiridas algunJugador}

adquirirPropiedad :: Propiedad -> Jugador -> Jugador 
adquirirPropiedad propiedad = propiedadComprada (propiedad :)

cobrarAlquileres :: Jugador->Jugador
cobrarAlquileres algunJugador= dinero (+gananciasPorCadaPropiedadBarata algunJugador)(dinero(+gananciasPorCadaPropiedadCara algunJugador) algunJugador)

propiedadBarata::Propiedad->Bool
propiedadBarata algunaPropiedad= precioDePropiedad algunaPropiedad < 150

propiedadCara:: Propiedad->Bool
propiedadCara algunaPropiedad= precioDePropiedad algunaPropiedad >=150

propiedadesBaratasAdquiridas::Jugador->Int
propiedadesBaratasAdquiridas algunJugador = length. filter propiedadBarata.propiedadesAdquiridas $algunJugador

propiedadesCarasAdquiridas:: Jugador ->Int
propiedadesCarasAdquiridas algunJugador =  length. filter propiedadCara .propiedadesAdquiridas   $algunJugador

gananciasPorCadaPropiedadBarata::Jugador -> Int
gananciasPorCadaPropiedadBarata= (*10).propiedadesBaratasAdquiridas

gananciasPorCadaPropiedadCara:: Jugador ->Int
gananciasPorCadaPropiedadCara = (*20).propiedadesCarasAdquiridas

pagarAAccionistas:: Acciones
pagarAAccionistas algunJugador
	|   esAccionista algunJugador = dinero (+100) algunJugador
	|   otherwise                 = dinero (subtract 100) algunJugador

hacerBerrinchePor:: Propiedad -> Acciones
hacerBerrinchePor algunaPropiedad
   |   aceptacionDeCompra algunaPropiedad algunJugador = adquirirPropiedad algunaPropiedad (dinero (subtract (precioDePropiedad algunaPropiedad)) algunJugador
   |   otherwise                                       =  hacerBerrinchePor algunaPropiedad (gritar.dinero(+10))

aceptacionDeCompra:: Propiedad->Jugador->Bool
aceptacionDeCompra algunaPropiedad algunJugador= precioDePropiedad algunaPropiedad> cantidadDeDinero algunJugador

ultimaRonda:: Acciones
ultimaRonda algunJugador = foldr ($) algunJugador  $accionesRealizadas algunJugador

juegoFinal:: Jugador->Jugador->String
juegoFinal primerJugador segundoJugador
  |  dineroRestante primerJugador > dineroRestante segundoJugador
  |  otherwise = nombre segundoJugador

dineroRestante::Jugador->Int
dineroRestante = cantidadDeDinero.ultimaRonda