package dominio

import org.junit.Test
import org.junit.Assert._

import scala.util.Success
import scala.util.Failure

class VikingosJinetesDragonesTests {
  
  //PRECONDICIONES
  import dominio.Precondiciones._
  
  
  //TESTS
  @Test
  def unDragonQuePesa100PuedeLlevarAUnVikingoQuePesa20 {
    assertTrue(DragonTest(peso = 100).puedeLlevar(VikingoTest(peso = 20)))
  }
  
  @Test
  def unDragonQuePesa100NoPuedeLlevarAUnVikingoQuePesaMasDe20 {
    assertFalse(DragonTest(peso = 100).puedeLlevar(VikingoTest(peso = 21)))
  }
  
  @Test
  def unFuriaNocturnaTieneElTripleDeVelocidadQueOtrosDragones {
    //un dragon comun, tendria 50
    assertEquals(150, FuriaNocturna(100,50,0).velocidad)
  }
  
  @Test
  def requisitoBarbarosidadMinima {
    
    val dragon: Dragon = DragonTest(requisitos = List(TieneBarbarosidadMinima(10)))
  
    assertTrue(dragon.acepta(VikingoTest(barbarosidad = 11)))
    assertTrue(!dragon.acepta(VikingoTest(barbarosidad = 10)))
    assertTrue(!dragon.acepta(VikingoTest(barbarosidad = 9)))
  }
  
  @Test
  def requisitoTieneItem {
    
    val dragon: Dragon = DragonTest(requisitos = List(tieneItem(Arma(30))))
  
    assertTrue(!dragon.acepta(VikingoTest(item = SistemaDeVuelo)))
    assertTrue(dragon.acepta(VikingoTest(item = Arma(30))))
  }
  
  @Test
  def requisitoNoSuperaDanioDelDragon {
    
    val dragon: Dragon = DragonTest(danio = 10, requisitos = List(NoSuperaDanioPropio))
      
    assertTrue(dragon.acepta(VikingoTest(danioBase = 9)))
    assertTrue(dragon.acepta(VikingoTest(danioBase = 10)))
    assertTrue(!dragon.acepta(VikingoTest(danioBase = 11)))
  }
  
  @Test
  def vikingoMontaDragonConExito {
    
    val dragon: Dragon = DragonTest(peso = 100)
    
    val vikingoQuePuede: Vikingo = VikingoTest(peso = 20)
    val vikingoQuePuede2: Vikingo = VikingoTest(peso = 19)
      
    assertEquals(Success(Jinete(vikingoQuePuede, dragon)), vikingoQuePuede.montar(dragon))
    assertEquals(Success(Jinete(vikingoQuePuede2, dragon)), vikingoQuePuede2.montar(dragon))
  }
  
  @Test
  def vikingoMontaDragonSinExito {
    
     val dragon: Dragon = DragonTest(peso = 100)
    
     val vikingoQueNoPuede: Vikingo = VikingoTest(peso = 21)
     
     assertEquals(Failure(VikingoNoPuedeMontarDragonException(vikingoQueNoPuede, dragon)), vikingoQueNoPuede.montar(dragon))
  }
  
  @Test
  def vikingoParticipaDePescaYAumentaSuHambreCincoPorciento {
    
    val vikingo: Vikingo = VikingoTest(porcentajeHambre = 100)
    
    assertEquals(VikingoTest(porcentajeHambre = 105), vikingo.__participar(Pesca()))
  }
  
  @Test
  def vikingoParticipaDeCombateYAumentaSuHambreDiezPorciento {
    
    val vikingo: Vikingo = VikingoTest(porcentajeHambre = 100)
    
    assertEquals(VikingoTest(porcentajeHambre = 110), vikingo.__participar(Combate()))
  }
  
  @Test
  def vikingoParticipaDeCarreraYAumentaSuHambreIGualALaCantidadDeKms {
    
    val vikingo: Vikingo = VikingoTest()
    
    assertEquals(VikingoTest(porcentajeHambre = 100), vikingo.__participar(Carrera(100)))
  }
  
  @Test
  def JineteParticipaDeUnaPostaCualquieraYAumentaSuHambreCincoPorciento {
    
    val jinete: Jinete = Jinete(VikingoTest(porcentajeHambre = 100),DragonTest())
    
    assertEquals(Jinete(VikingoTest(porcentajeHambre = 105),DragonTest()), jinete.__participar(Pesca()))
    assertEquals(Jinete(VikingoTest(porcentajeHambre = 105),DragonTest()), jinete.__participar(Combate()))
    assertEquals(Jinete(VikingoTest(porcentajeHambre = 105),DragonTest()), jinete.__participar(Carrera(100)))
  }
  
  @Test
  def patapezParticipaDeUnaCarreraDe50KmsConUnComestibleDe70PorcientoSuHambreAlFinalEsDe30Porciento {
    
    val patapez: Vikingo = Patapez("",1,1,0,1,1,Comestible(70))
    
    assertEquals(Patapez("", 1,1,30,1,1,Comestible(70)), patapez.__participar(Carrera(50)))
  }
  
  @Test
  def pescaPuedeTenerUnRequisitoDePesoMinimoParaUnParticipante {
    
    //con 100 de barbarosidad levanta 200 kg de pescado
    val participanteQueCumple: ParticipanteIndividual = VikingoTest(barbarosidad = 100)
    //con 40 de barbarosidad levanta 80 kg de pescado
    val participanteQueNoCumple: ParticipanteIndividual = VikingoTest(barbarosidad = 40)
    
    //si tiene requisitos
    assertTrue(Pesca(Some(PuedeLevantar(100))).cumpleLosRequisitos(participanteQueCumple))
    assertTrue(!Pesca(Some(PuedeLevantar(100))).cumpleLosRequisitos(participanteQueNoCumple))
    
    //si no tiene requisitos
    assertTrue(Pesca().cumpleLosRequisitos(participanteQueCumple))
    assertTrue(Pesca().cumpleLosRequisitos(participanteQueNoCumple))
  }
 
  @Test
  def combateTieneDosRequisitosParaUnParticipante {
    
    val participanteQueCumple: ParticipanteIndividual = VikingoTest(barbarosidad = 100)
    val participanteQueCumple2: ParticipanteIndividual = VikingoTest(item = Arma(100))
    val participanteQueNoCumple: ParticipanteIndividual = VikingoTest()
    
    //si tiene requisitos
    assertTrue(Combate(List(TieneBarbosidad(100), TieneArma)).cumpleLosRequisitos(participanteQueCumple))
    assertTrue(Combate(List(TieneBarbosidad(100), TieneArma)).cumpleLosRequisitos(participanteQueCumple2))
    assertTrue(!Combate(List(TieneBarbosidad(100), TieneArma)).cumpleLosRequisitos(participanteQueNoCumple))
    
    //si no tiene requisitos
    assertTrue(Combate().cumpleLosRequisitos(participanteQueCumple))
    assertTrue(Combate().cumpleLosRequisitos(participanteQueCumple2))
    assertTrue(Combate().cumpleLosRequisitos(participanteQueNoCumple))
  }
  
  @Test
  def carreraPuedeTenerUnRequisitoDeTenerMonturaParaUnParticipante {
    
    val participanteQueCumple: ParticipanteIndividual = Jinete(VikingoTest(),DragonTest())
    val participanteQueNoCumple: ParticipanteIndividual = VikingoTest()
    
    //si tiene requisitos
    assertTrue(Carrera(100, Some(TieneMontura)).cumpleLosRequisitos(participanteQueCumple))
    assertTrue(!Carrera(100, Some(TieneMontura)).cumpleLosRequisitos(participanteQueNoCumple))
    
    //si no tiene requisitos
    assertTrue(Carrera(100).cumpleLosRequisitos(participanteQueCumple))
    assertTrue(Carrera(100).cumpleLosRequisitos(participanteQueNoCumple))
  }
  
  @Test
  def vikingoQuedariaKnockOutSiParticiparaDeUnPostaQueAumentaraSuHambreHasta100 {
    
    val vikingo: Vikingo = VikingoTest()
    
    assertTrue(vikingo.quedariaKnockOutSiParticipara(Carrera(101)))
    assertTrue(vikingo.quedariaKnockOutSiParticipara(Carrera(100)))
    assertTrue(!vikingo.quedariaKnockOutSiParticipara(Carrera(99)))
  }
  
  @Test
  def dadosDosVikingosYUnaPostaSaberCualEsElMejorParaLaPosta {
    
    val vikingoQueHaceMuchoDanio: Vikingo = VikingoTest(danioBase = 50)
    val vikingoQueHaceMismoDanio: Vikingo = VikingoTest(danioBase = 50)
    val vikingoQueHacePocoDanio: Vikingo = VikingoTest(danioBase = 10)
    
    assertTrue(vikingoQueHaceMuchoDanio.esMejorQue(vikingoQueHacePocoDanio, Combate()))
    assertTrue(!vikingoQueHacePocoDanio.esMejorQue(vikingoQueHaceMuchoDanio, Combate()))
    
    assertTrue(!vikingoQueHaceMismoDanio.esMejorQue(vikingoQueHaceMuchoDanio, Combate()))
    assertTrue(!vikingoQueHaceMuchoDanio.esMejorQue(vikingoQueHaceMismoDanio, Combate()))
  }
  
  @Test
  def aUnVikingoRapidoNoLeConvieneParticiparConUnDragonLentoEnUnaCarrera {
    
    val dragonLento: Dragon = DragonTest(peso = 40) //velocidad = 20
    val vikingoRapido: Vikingo = VikingoTest(velocidad = 40)
    
    assertEquals(Some(vikingoRapido), vikingoRapido.comoLeConvieneParticipar(Carrera(10), List(dragonLento))) 
  }
  
  @Test
  def aUnVikingoLentoLeConvieneParticiparConUnDragonRapidoEnUnaCarrera {
    
    val dragonRapido: Dragon = DragonTest() //velocidad = 60
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    
    assertEquals(Some(Jinete(vikingoLento, dragonRapido)), vikingoLento.comoLeConvieneParticipar(Carrera(10), List(dragonRapido))) 
  }
  
  @Test
  def aUnVikingoLentoLeConvieneParticiparComoVikingoEnUnaCarreraSiNoHayDragonesDisponibles {
    
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    
    assertEquals(Some(vikingoLento), vikingoLento.comoLeConvieneParticipar(Carrera(10), List())) 
  }
  
  @Test
  def vikingoNoeConvieneParticiparDeNingunaFormaPorqueNoCumpleLasCondicionesDeLaPosta {
    
    val vikingo: Vikingo = VikingoTest()
    
    assertEquals(None, vikingo.comoLeConvieneParticipar(Carrera(10, Option(TieneMontura)), List())) 
  }
  
  @Test
  def aUnVikingoLeConvieneParticiparConElDragonMasRapidoEnUnaCarrera {
    
    val dragonLento: Dragon = DragonTest() //velocidad = 60
    val dragonRapido: Dragon = DragonTest(velocidadBase = 120) //velocidad = 120
    
    val vikingoLento: Vikingo = VikingoTest(velocidad = 10)
    
    assertEquals(Some(Jinete(vikingoLento, dragonRapido)), vikingoLento.comoLeConvieneParticipar(Carrera(10), List(dragonRapido, dragonLento))) 
  }
}