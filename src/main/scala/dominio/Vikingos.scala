package dominio

import scala.util.Try
import scala.util.Success
import scala.util.Failure

//VIKINGO
trait Vikingo extends ParticipanteIndividual {
  
  def nombre: String
  def peso:Int
  def velocidad: Int
  def porcentajeHambre: Double
  def barbarosidad:Int
  def danioBase: Int
  def item: Item

  def danio: Int = { 
    item match { 
      case Arma(danio) => danio 
      case _ => 0
    }
    + danioBase
  }
  
  def aumentarHambre(cantidad: Double): Vikingo
  
  def disminuirHambre(cantidad: Double): Vikingo
  
  def quedariaKnockOutSiParticipara(unaPosta: Posta): Boolean = unaPosta.cantidadHambreQueDariaAlParticipar(this) + porcentajeHambre >= 100
  
  def __participar(unaPosta: Posta): Vikingo = aumentarHambre(unaPosta.cantidadHambreQueDariaAlParticipar(this))
  
  def montar(unDragon: Dragon): Try[Jinete] = if(unDragon.puedeLlevar(this)) Success(Jinete(this,unDragon)) else Failure(VikingoNoPuedeMontarDragonException(this,unDragon))
  
  def cantidadPescadoQueLevanta: Double = 0.5 * peso + 2 * barbarosidad
  
  def tieneArma: Boolean = item.isInstanceOf[Arma]
  
  def comoLeConvieneParticipar(unaPosta: Posta, dragones: List[Dragon]): Option[ParticipanteIndividual] = {
    
    val posiblesJinetes: List[Try[Jinete]] = dragones.map(dragon => this.montar(dragon)).filter(_.isSuccess)
    
    val posiblesParticipantes = posiblesJinetes.map(_.get).::(this)
    
    unaPosta.elQueGanaria(posiblesParticipantes)
  }
  
  def vikingo: Vikingo = this
  
  def esIgualA(otroVikingo: Vikingo) = this.nombre == otroVikingo.nombre
  
  def perteneceA(unEquipo: Equipo) = unEquipo.esDelEquipo(this)
}


case class VikingoComun(nombre: String, peso:Int, velocidad: Int, porcentajeHambre: Double, barbarosidad:Int, danioBase: Int, item: Item) extends Vikingo {
  
  def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + cantidad)
  
  def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
}


case class Hipo(nombre: String = "Hipo", peso:Int, velocidad: Int, porcentajeHambre: Double, barbarosidad:Int, danioBase: Int, item: Item = SistemaDeVuelo) extends Vikingo {
  
 def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + cantidad)
 
 def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
}


case class Astrid(nombre: String = "Astrid", peso:Int, velocidad: Int, porcentajeHambre: Double, barbarosidad:Int, danioBase: Int, item: Item = Arma(30)) extends Vikingo {
  
  def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + cantidad)
  
  def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
}


case class Patan(nombre: String = "Patan", peso:Int, velocidad: Int, porcentajeHambre: Double, barbarosidad:Int, danioBase: Int, item: Item = Arma(100)) extends Vikingo {
  
 def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + cantidad)
 
 def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
}


case class Patapez(nombre: String = "Patapez", peso:Int, velocidad: Int, porcentajeHambre: Double, barbarosidad: Int, danioBase: Int, item: Comestible) extends Vikingo {
  
  def tieneMuchaHambre: Boolean = porcentajeHambre >= 50
  
  def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + 2 * cantidad)
  
  override def puedeParticipar(unaPosta: Posta): Boolean = super.puedeParticipar(unaPosta) && !tieneMuchaHambre
  
  override def __participar(unaPosta: Posta): Vikingo = super.__participar(unaPosta).disminuirHambre(item.cantidadEnergia)
  
  def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
}




//ITEM
trait Item

case class Arma(val danio: Int) extends Item

case object SistemaDeVuelo extends Item

case class Comestible(cantidadEnergia: Int) extends Item



//EXCEPCIONES
case class VikingoNoPuedeMontarDragonException(unVikingo: Vikingo, unDragon: Dragon) extends Exception