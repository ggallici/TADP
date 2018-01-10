package dominio

import dominio.Tipos._

//DRAGON
trait Dragon {

  //por si despues no funciona, esto era un val
  def requisitos: List[RequisitoMontaje] = Nil

  def velocidadBase: Int = 60
  def peso: Int
  def danio: Int
  
  def velocidad = velocidadBase - peso
  
  def puedeLlevar(unVikingo: Vikingo): Boolean = puedeCargar(unVikingo) && acepta(unVikingo)
  
  def puedeCargar(unVikingo: Vikingo): Boolean = unVikingo.peso <= pesoMaximoQuePuedeLlevar

  def pesoMaximoQuePuedeLlevar = 0.2 * peso
  
  def acepta(unVikingo: Vikingo): Boolean = requisitos.forall(_.apply(unVikingo,this))
}

case class FuriaNocturna(override val velocidadBase: Int, peso: Int, danio: Int, override val requisitos: List[RequisitoMontaje] = Nil) extends Dragon { override def velocidad = super.velocidad * 3 }

case class NadderMortifero(override val velocidadBase: Int, peso: Int, requisitosExtra: List[RequisitoMontaje] = Nil) extends Dragon { 
  
  def danio = 150 
  
  override def requisitos = requisitosExtra.:+(NoSuperaDanioPropio) 
}

case class Gronckle(peso: Int, pesoMaximoDeVikingoQueAcepta: Int, requisitosExtra: List[RequisitoMontaje] = Nil) extends Dragon {
  
  override def velocidadBase: Int = super.velocidadBase / 2
  
  def danio = 5 * peso
  
  override def requisitos = requisitosExtra.:+(NoSuperaPeso(pesoMaximoDeVikingoQueAcepta))
}



//REQUISITOS DE MONTAJE
case class TieneBarbarosidadMinima(barbarosidadRequerida: Int) extends RequisitoMontaje {
  def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = unVikingo.barbarosidad > barbarosidadRequerida
}

case class tieneItem(itemRequerido: Item) extends RequisitoMontaje {
  def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = unVikingo.item == itemRequerido
}

case class NoSuperaPeso(pesoRequerido: Int) extends RequisitoMontaje {
  def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = unVikingo.peso <= pesoRequerido
}

case object NoSuperaDanioPropio extends RequisitoMontaje {
  def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = unVikingo.danio <= unDragon.danio
}