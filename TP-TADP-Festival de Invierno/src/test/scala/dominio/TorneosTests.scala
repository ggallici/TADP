package dominio

import org.junit.Test
import org.junit.Assert._

class TorneosTests {
  
  //PRECONDICIONES
  import dominio.Precondiciones._
  
  
  //TESTS
  @Test
  def vikingoLentoEligePrimeroYElDragonRapidoYaNoEstaDisponible {
    
    val dragonLento: Dragon = DragonTest() //velocidad = 60
    val dragonRapido: Dragon = DragonTest(velocidadBase = 120) //velocidad = 120
    
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    val vikingoRapido: Vikingo = VikingoTest(velocidad = 20)
    
    val vikingos: List[Vikingo] = List(vikingoLento, vikingoRapido)
    val dragones: List[Dragon] = List(dragonLento, dragonRapido)
    
    val esperado: List[ParticipanteIndividual] = List(Jinete(vikingoLento,dragonRapido), Jinete(vikingoRapido,dragonLento))
    
    assertEquals(esperado, Estandar.elegirDragones(vikingos, Carrera(10), dragones)) 
  }
  
  @Test
  def vikingoRapidoEligePrimeroPeroParticipaComoVikingoPorLoTantoLosDragonesSiguenDisponibles {
    
    val dragonLento: Dragon = DragonTest() //velocidad = 60
    val dragonRapido: Dragon = DragonTest(velocidadBase = 120) //velocidad = 120
    
    val vikingoMuyRapido: Vikingo = VikingoTest(velocidad = 150)
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    
    val vikingos: List[Vikingo] = List(vikingoMuyRapido, vikingoLento)
    val dragones: List[Dragon] = List(dragonLento, dragonRapido)
    
    val esperado: List[ParticipanteIndividual] = List(vikingoMuyRapido, Jinete(vikingoLento,dragonRapido))
    
    assertEquals(esperado, Estandar.elegirDragones(vikingos, Carrera(10), dragones)) 
  }
  
  @Test
  def SiSeAcabanLosDragonesSeParticipaComoJineteAunqueNoSeQuiera {
    
    val dragonLento: Dragon = DragonTest() //velocidad = 60
    val dragonRapido: Dragon = DragonTest(velocidadBase = 120) //velocidad = 120
    
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    val vikingoLento1: Vikingo = VikingoTest(velocidad = 10)
    val vikingoLento2: Vikingo = VikingoTest(velocidad = 10)

    val vikingos: List[Vikingo] = List(vikingoLento, vikingoLento1, vikingoLento2)
    val dragones: List[Dragon] = List(dragonLento, dragonRapido)
    
    val esperado: List[ParticipanteIndividual] = List(Jinete(vikingoLento,dragonRapido), Jinete(vikingoLento1,dragonLento), vikingoLento2)
    
    assertEquals(esperado, Estandar.elegirDragones(vikingos, Carrera(10), dragones)) 
  }

  @Test
  def torneoEstandarSiParticipaUnoSoloEsElGanadorSinJugarNingunaPosta {

    //el vikingo no podria jugar la carrera porque no tiene montura pero como es el unico gana igual
    val vikingo: Vikingo = VikingoTest()

    assertEquals(Some(vikingo), TorneoEstandar(Estandar,List(Carrera(10,Option(TieneMontura))), List()).ganador(List(vikingo)))
  }
  
  @Test
  def torneoEstandarSiParticipaDosPeroNoHayMasPostasElGanadorSeDecideSegunLasReglas {
    //la regla en este caso selecciona al primero en anotarse
    
    val vikingo1: Vikingo = VikingoTest()
    val vikingo2: Vikingo = VikingoTest()

    val vikingos = List(vikingo1, vikingo2)
    
    assertEquals(Some(vikingo1), TorneoEstandar(Estandar,List(), List()).ganador(vikingos))
  }
  
  @Test
  def torneoEstandarSiNoHayVikingosNoHayGanador {
    
    assertEquals(None, TorneoEstandar(Estandar,List(), List()).ganador(List()))
  }
  
  @Test
  def torneoPorEquiposSiParticipaUnoSoloEsElGanadorSinJugarNingunaPosta {
    
    val equipo = Equipo(List(VikingoTest()))

    assertEquals(Some(equipo), TorneoPorEquipos(PorEquipos, List(Carrera(10,Option(TieneMontura))), List()).ganador(List(equipo)))
  }
  
  @Test
  def torneoPorEquiposSiParticipanDosPeroNoHayMasPostasElGanadorSeDecideSegunLasReglas {
    //la regla en este caso selecciona al que mas tenga jugadores
    
    val equipo1 = Equipo(List(VikingoTest(), VikingoTest()))
    val equipo2 = Equipo(List(VikingoTest(), VikingoTest(), VikingoTest()))

    val equipos = List(equipo1, equipo2)
    
    assertEquals(Some(equipo2), TorneoPorEquipos(PorEquipos,List(), List()).ganador(equipos))
  }
  
  @Test
  def torneoPorEquiposSiNoHayEquiposNoHayGanador {
    
    assertEquals(None, TorneoPorEquipos(PorEquipos,List(), List()).ganador(List()))
  }
  
  @Test
  def torneoPorEquiposReagruparVikingosMescladosEnSusCorrespondientesEquipos {
    
    val vikingo1 = VikingoTest(nombre = "v1")
    val vikingo2 = VikingoTest(nombre = "v4")
    val vikingo3 = VikingoTest(nombre = "v5")
    val vikingo4 = VikingoTest(nombre = "v6")
    val vikingo5 = VikingoTest(nombre = "v7")
    val vikingo6 = VikingoTest(nombre = "v8")
    
    val equipoA = Equipo(List(vikingo1))
    val equipoB = Equipo(List(vikingo6, vikingo3))
    val equipoC = Equipo(List(vikingo4,vikingo5, vikingo2))
    val equipoD = Equipo(List())
    
    val vikingosMesclados = List(vikingo3,vikingo6,vikingo4,vikingo2,vikingo5)
    val equiposViejos = List(equipoA,equipoB,equipoC,equipoD)
    
    val esperado = List(Equipo(List(vikingo6, vikingo3)), Equipo(List(vikingo4,vikingo5, vikingo2)))
    
    assertEquals(esperado, TorneoPorEquipos(null, null, null).reagrupar(vikingosMesclados, equiposViejos))
  }
}