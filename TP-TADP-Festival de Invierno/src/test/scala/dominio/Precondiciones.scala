package dominio

import org.junit.BeforeClass

import dominio.Tipos._

object Precondiciones {
  
  
  @BeforeClass
  case class ItemTest() extends Item
  
  @BeforeClass
  case class DragonTest(danio: Int = 0, peso: Int = 0, override val velocidadBase: Int = 60, override val requisitos: List[RequisitoMontaje] = Nil) extends Dragon
  
  @BeforeClass
  case class VikingoTest(nombre: String = "VikingoTest", peso:Int = 0, velocidad: Int = 0, porcentajeHambre: Double = 0, barbarosidad:Int = 0, danioBase: Int = 0, item: Item = ItemTest()) extends Vikingo {
    def aumentarHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre + cantidad)
    def disminuirHambre(cantidad: Double): Vikingo = copy(porcentajeHambre = porcentajeHambre - cantidad)
  }
}