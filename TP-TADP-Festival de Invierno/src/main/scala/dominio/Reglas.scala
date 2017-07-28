package dominio

trait ReglaGeneral {
  
	def ordenar(vikingos: List[Vikingo]): List[Vikingo]
  
	def filtrarLosQuePasanDePosta(participantesMesclados: List[ParticipanteIndividual]): List[ParticipanteIndividual] 
  
  def elegirDragones(vikingos: List[Vikingo], unaPosta: Posta, dragonesDisponibles: List[Dragon]): List[ParticipanteIndividual] = {
	  
		var dragonesQueQuedan = dragonesDisponibles
	  
		vikingos.foldLeft(List[ParticipanteIndividual]()) {  (participantesIndividuales, vikingo) =>   
	     
	    val participanteOption = vikingo.comoLeConvieneParticipar(unaPosta, dragonesQueQuedan)
	    
	    if(!participanteOption.isEmpty) {
	      
	      val participante = participanteOption.get
	      
	      participante match {
	        
	        case Jinete(_,dragon) => dragonesQueQuedan = dragonesQueQuedan.filter(d => d != dragon) 
	        
	        case _ => dragonesQueQuedan
	      }
	      
	      participantesIndividuales :+ participante
	    }
	    else participantesIndividuales
	  }
	}
}



///////////////////////////////////////////////////////
/////////////////////INDIVIDUALES/////////////////////
//////////////////////////////////////////////////////

trait ReglaIndividual extends ReglaGeneral{
  
  def ordenar(vikingos: List[Vikingo]): List[Vikingo] = vikingos
  
  def filtrarLosQuePasanDePosta(participantes: List[ParticipanteIndividual]): List[ParticipanteIndividual] = participantes.take(participantes.length / 2)
  
  def seleccionarGanador(vikingos: List[Vikingo]): Option[Vikingo] = vikingos.headOption 
}

case object Estandar extends ReglaIndividual

case class Eliminacion(cantidadEliminados: Int) extends ReglaIndividual {
  
  override def filtrarLosQuePasanDePosta(participantes: List[ParticipanteIndividual]) = participantes.dropRight(cantidadEliminados)
}


case object TorneoInverso extends ReglaIndividual {
  
  override def filtrarLosQuePasanDePosta(participantes: List[ParticipanteIndividual]) = participantes.takeRight(participantes.length / 2)
  
  override def seleccionarGanador(vikingos: List[Vikingo]) = vikingos.lastOption
}


case class ConBanDeDragones(condicionQueDebenCumplir: Dragon => Boolean) extends ReglaIndividual {
  
  override def elegirDragones(vikingos: List[Vikingo], unaPosta: Posta, dragonesDisponibles: List[Dragon]): List[ParticipanteIndividual] = {
    
    val dragonesQueCumplen = dragonesDisponibles.filter(condicionQueDebenCumplir)
    
    super.elegirDragones(vikingos, unaPosta, dragonesQueCumplen)
  }
}


case object ConHandicap extends ReglaIndividual {
  
  override def ordenar(vikingos: List[Vikingo]): List[Vikingo] = super.ordenar(vikingos.reverse)
}





//////////////////////////////////////////////////////
/////////////////////PARA EQUIPOS/////////////////////
//////////////////////////////////////////////////////

trait ReglaPorEquipos extends ReglaGeneral {
  
  def ordenar(vikingos: List[Vikingo]): List[Vikingo] = vikingos
  
  def filtrarLosQuePasanDePosta(participantesMesclados: List[ParticipanteIndividual]): List[ParticipanteIndividual] = participantesMesclados.take(participantesMesclados.length / 2)
  
  def seleccionarGanador(equipos: List[Equipo]): Option[Equipo] = if(!equipos.isEmpty) Some(equipos.maxBy(_.cantidadVikingos)) else None
}

case object PorEquipos extends ReglaPorEquipos