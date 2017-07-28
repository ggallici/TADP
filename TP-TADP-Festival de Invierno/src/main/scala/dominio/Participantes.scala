package dominio


trait Participante 

case class Equipo(vikingos: List[Vikingo]) extends Participante {
  
  def cantidadVikingos: Int = vikingos.size
  
  def modificarVikingos(vikingosNuevos: List[Vikingo]): Equipo = copy(vikingos = vikingosNuevos)
  
  def esDelEquipo(unVikingo: Vikingo) = vikingos.exists(vikingo => vikingo.esIgualA(unVikingo))
  
  def nuevosVikingos(vikingosNuevos: List[Vikingo]) = copy(vikingos = vikingos)
}

trait ParticipanteIndividual extends Participante {
  
  def cantidadPescadoQueLevanta: Double
  
  def porcentajeHambre: Double
   
  def danio: Int
  
  def velocidad: Int
  
  def tieneArma: Boolean
  
  def barbarosidad: Int
  
  def quedariaKnockOutSiParticipara(unaPosta: Posta): Boolean
  
  def puedeParticipar(unaPosta: Posta): Boolean = unaPosta.cumpleLosRequisitos(this) && !quedariaKnockOutSiParticipara(unaPosta)
  
  def aumentarHambre(cantidad: Double): ParticipanteIndividual
  
  def disminuirHambre(cantidad: Double): ParticipanteIndividual
  
  //SALTEA EL CHECKEO DE SI PUEDE O NO PARTICIPAR(EN POSTA)
  def __participar(unaPosta: Posta): ParticipanteIndividual
  
  def esMejorQue(unParticipanteIndividual: ParticipanteIndividual, unaPosta: Posta): Boolean = unaPosta.esMejorParaLaPosta(this, unParticipanteIndividual)
  
  def vikingo: Vikingo
}