package dominio

trait TorneoPiola {
  
  def regla: ReglaGeneral
  def postas: List[Posta]
  def dragonesDisponibles: List[Dragon]
  
  def jugarPosta(vikingos: List[Vikingo], posta: Posta) = {
    
    val ordenados = regla.ordenar(vikingos)

    val preparados = regla.elegirDragones(ordenados, posta, dragonesDisponibles)

    val losQueSuperaronLaPosta = posta.competir(preparados)

    val losQuePasaronElFiltro = regla.filtrarLosQuePasanDePosta(losQueSuperaronLaPosta)

    losQuePasaronElFiltro.map(_.vikingo)
  }
}


case class TorneoEstandar(regla: ReglaIndividual, postas: List[Posta], dragonesDisponibles: List[Dragon]) extends TorneoPiola {
  
  def ganador(vikingos: List[Vikingo]): Option[Vikingo] = {

    val finalistas = postas.foldLeft(vikingos) { (vikingos, posta) => if(vikingos.length == 1) return vikingos.headOption else jugarPosta(vikingos, posta) }
    
    regla.seleccionarGanador(finalistas)
  }
}



case class TorneoPorEquipos(regla: ReglaPorEquipos, postas: List[Posta], dragonesDisponibles: List[Dragon]) extends TorneoPiola {

  def ganador(equipos: List[Equipo]): Option[Equipo] = {

    val equiposFinalistas = postas.foldLeft(equipos) { (equiposViejos, posta) =>

      if (equiposViejos.length == 1) return equiposViejos.headOption else {

        //mesclo todos los vikingos de todos los equipos
        val vikingosMesclados = equiposViejos.flatMap(equipo => equipo.vikingos)
        
        val losVikingos = jugarPosta(vikingosMesclados, posta)

        reagrupar(losVikingos, equiposViejos)
      }
    }
    regla.seleccionarGanador(equiposFinalistas)
  }

  def reagrupar(vikingos: List[Vikingo], equiposViejos: List[Equipo]): List[Equipo] = {

    equiposViejos.foldLeft(List[Equipo]()) { (equiposNuevos, equipoViejo) =>

      val vikings = vikingos.filter(vikingo => vikingo.perteneceA(equipoViejo))

      if (!vikings.isEmpty) {

        val equipoNuevo = equipoViejo.nuevosVikingos(vikings)

        equiposNuevos :+ equipoNuevo
      } 
      else equiposNuevos
    }
  }
}