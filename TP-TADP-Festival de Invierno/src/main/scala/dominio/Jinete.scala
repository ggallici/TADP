package dominio

case class Jinete (vikingo: Vikingo, dragon: Dragon) extends ParticipanteIndividual {
  
  def cantidadPescadoQueLevanta: Double = dragon.pesoMaximoQuePuedeLlevar - vikingo.peso
  
  def tieneArma: Boolean = vikingo.tieneArma
  
  def porcentajeHambre: Double = vikingo.porcentajeHambre
  
  def barbarosidad: Int = vikingo.barbarosidad
  
  def danio: Int = vikingo.danio + dragon.danio
  
  def velocidad: Int = dragon.velocidad - vikingo.peso
  
  def quedariaKnockOutSiParticipara(unaPosta: Posta): Boolean = cantidadHambreQueDariaAlParticipar(unaPosta) + porcentajeHambre >= 100
  
  def cantidadHambreQueDariaAlParticipar(unaPosta: Posta): Double = porcentajeHambre * 0.05
  
  def aumentarHambre(cantidad: Double): Jinete = copy(vikingo = vikingo.aumentarHambre(cantidad))
  
  def disminuirHambre(cantidad: Double): Jinete = copy(vikingo = vikingo.disminuirHambre(cantidad))
  
  def __participar(unaPosta: Posta): Jinete = aumentarHambre(cantidadHambreQueDariaAlParticipar(unaPosta))
}