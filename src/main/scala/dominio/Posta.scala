package dominio

import dominio.Tipos._

trait Posta {
  
  def cumpleLosRequisitos(unParticipante: ParticipanteIndividual): Boolean
  
  def cantidadHambreQueDariaAlParticipar(unVikingo: Vikingo): Double
  
  def esMejorParaLaPosta(unParticipante: ParticipanteIndividual, otroParticipante: ParticipanteIndividual): Boolean
  
  //hay logica repetida entre competir y elQueGanaria, pero uno simula tener efecto y el otro no
  //Que pasaria si el que es mejor para la posta, es el que luego de participar quedo con menos hambre???
  def competir(participantes: List[ParticipanteIndividual]): List[ParticipanteIndividual] = participantes
                                                                                           .filter(_.puedeParticipar(this))
                                                                                           .map(_.__participar(this))
                                                                                           .sortWith(esMejorParaLaPosta)
                                                                       
  def elQueGanaria(participantes: List[ParticipanteIndividual]): Option[ParticipanteIndividual] = participantes
                                                                                                 .filter(_.puedeParticipar(this))
                                                                                                 .sortWith(esMejorParaLaPosta)
                                                                                                 .headOption
}

case class Pesca(requisito: Option[RequisitoParticipacion] = None) extends Posta {
    
	def cumpleLosRequisitos(unParticipante: ParticipanteIndividual): Boolean =  requisito.forall(_.apply(unParticipante))
  
	def cantidadHambreQueDariaAlParticipar(unVikingo: Vikingo): Double = unVikingo.porcentajeHambre * 0.05
	
	def esMejorParaLaPosta(unParticipante: ParticipanteIndividual, otroParticipante: ParticipanteIndividual): Boolean = unParticipante.cantidadPescadoQueLevanta > otroParticipante.cantidadPescadoQueLevanta
}

case class Combate(requisitos: List[RequisitoParticipacion] = Nil) extends Posta {
  
  def cumpleLosRequisitos(unParticipante: ParticipanteIndividual): Boolean =  requisitos.isEmpty || requisitos.exists(_.apply(unParticipante))
  
  def cantidadHambreQueDariaAlParticipar(unVikingo: Vikingo): Double = unVikingo.porcentajeHambre * 0.1
  
  def esMejorParaLaPosta(unParticipante: ParticipanteIndividual, otroParticipante: ParticipanteIndividual): Boolean = unParticipante.danio > otroParticipante.danio
}

case class Carrera(cantidadKilometros: Int, requisito: Option[RequisitoParticipacion] = None) extends Posta {
  
  def cumpleLosRequisitos(unParticipante: ParticipanteIndividual): Boolean =  requisito.forall(_.apply(unParticipante))
  
  def cantidadHambreQueDariaAlParticipar(unVikingo: Vikingo): Double = cantidadKilometros
  
  def esMejorParaLaPosta(unParticipante: ParticipanteIndividual, otroParticipante: ParticipanteIndividual): Boolean = unParticipante.velocidad > otroParticipante.velocidad
}



//REQUISITOS DE PARTICIPACION
case class PuedeLevantar(pesoRequerido: Int) extends RequisitoParticipacion {
  def apply(unParticipante: ParticipanteIndividual): Boolean = unParticipante.cantidadPescadoQueLevanta >= pesoRequerido
}

case class TieneBarbosidad(barbarosidadRequerida: Int) extends RequisitoParticipacion {
  def apply(unParticipante: ParticipanteIndividual): Boolean  = unParticipante.barbarosidad >= barbarosidadRequerida
} 

case object TieneArma extends RequisitoParticipacion {
  def apply(unParticipante: ParticipanteIndividual): Boolean = unParticipante.tieneArma
}

case object TieneMontura extends RequisitoParticipacion {
  def apply(unParticipante: ParticipanteIndividual): Boolean = unParticipante.isInstanceOf[Jinete]
}