require 'rspec'
require './inmutabilidad'


include Inmutabilidad
include Pattern_Matching


describe 'Punto 1' do

  it 'Se puede crear una case_class y no explota todo' do

    class B
      def cumbia
        'cumbia'
      end
    end

    module M
      def metodoM
        'soym'
      end
    end

    case_class Apellido < B do

      def m1
        5
      end

      include M

    end

    a = Apellido.new

    expect(a.m1).to eq 5

    expect(a.cumbia).to eq 'cumbia'

    expect(a.metodoM).to eq 'soym'
  end


  it 'Todos los casos de herencia' do
    expect{case_class AX < BX do;end}.to raise_error TypeError, 'superclass must be a Class'

    case_class AX do
    end
    expect(AX.new.class.is_a? Class).to be true

    class JJ
    end
    case_class NN < JJ do
    end
    expect(NN.superclass).to be JJ
  end


  it 'Una case class No deberia permitir que otra clase herede de ella' do

    case_class ClaseTest4 do
      attr_accessor :nombre
    end

    expect{
      class CaseClassTest4 < ClaseTest4
        attr_accessor :atributo
      end
    }.to raise_error NotImplementedError,'No se puede heredar de una Case Class'

  end


  it 'se puede crear una case class sin inizializador' do

    case_class A2 do
      attr_accessor :nombre, :nota
    end

    a = A2('hola', 5)

    expect(a.nota).to eq 5
    expect(a.nombre).to eq 'hola'

  end

  it 'El attr_accessors de una case class solo genera getters' do

    case_class Klase do
      attr_accessor :nombre
    end

    a = Klase.new 'german'

    expect{a.nombre= ''}.to raise_error(NoMethodError)
  end

  it 'Las instancias de una Case class son inmutables' do

    case_class ClaseNueva do
      attr_accessor :nombre, :nota

      def cambiarNombre(nombre)
        @nombre = nombre
      end
    end

    a = ClaseNueva('gercho', 5)
    b = ClaseNueva.new('gercho', 5)

    expect{a.cambiarNombre 'pepito'}.to raise_error RuntimeError, 'can\'t modify frozen ClaseNueva'
    expect{b.cambiarNombre 'jorgito'}.to  raise_error RuntimeError, 'can\'t modify frozen ClaseNueva'

  end

  it 'Comparacion con == funciona' do

    case_class Sarasa do
      attr_accessor :id, :nombre
    end

    case_class Pindonga do
      attr_accessor :id, :nombre
    end

    a = Sarasa( 10, 'pepe')
    b = Sarasa(10, 'pepe')
    c = Pindonga(10, 'pepe')
    d = Sarasa(5, 'pepe')

    expect(a== b).to be true
    expect(a== d).to be false
    expect(a== c).to be false
  end

  it 'ToString en case class' do
    case_class Mm do
      attr_accessor :id, :nombre
    end

    a = Mm(123, 'MMMMMMM')

    expect(a.to_s).to eq ('Mm(123, MMMMMMM)')
  end

  it 'HAY QE PROBAR EL HASSSSSHHHH' do
    case_class Alumno do
      attr_accessor :nombre, :nota
    end

    alumno = Alumno("Jose", 8)
  end

  it 'hash, to_s, == son por defecto y no sobreescrien nada' do
    module M
      def to_s
        "Soy un M"
      end
    end

    class C
      def to_s
        "Soy un C"
      end
    end

    case_class X <  C do

    end

    case_class Y do
      include M
    end

    case_class Z do
      def to_s
        "Soy un Z"
      end
    end

    expect(X().to_s).to eq('Soy un C')
    expect(Y().to_s).to eq('Soy un M')
    expect(Z().to_s).to eq('Soy un Z')
  end

  it 'Copiado inteligente simple' do
    case_class Perro do
      attr_accessor :nombre
      def ladrar
        'guaguau'
      end
    end

    lazy = Perro('Lazy')

    otro_perro = lazy.copy

    expect(otro_perro == lazy).to be true

  end

  it 'Copiado inteligente con expresiones lambda' do
    case_class PerroLambda do
      attr_accessor :nombre, :edad
      def ladrar
        'guaguau'
      end
    end

    lazy = PerroLambda('Lazy', 10)

    otro_perro = lazy.copy -> (nombre) {'Perro Dinamita'}, ->(edad) {edad / 2}

    expect(otro_perro).to eq PerroLambda('Perro Dinamita', 5)
  end
end

describe 'Punto 2' do

  it 'Probando los case_objects' do

    case_class Termino do
      attr_accessor :nota
    end

    case_object Cursando do
      def estado
        'cursando muy duro, como un esclavo'
      end
    end

    case_class Estudiante do
      attr_accessor :id, :nombre
    end
    estudiante = Estudiante(123, Termino(10))
    estudiante2 = Estudiante(456, Cursando)

    expect(estudiante).to eq (Estudiante(123, Termino(10)))
    expect(estudiante2).to eq(Estudiante(456, Cursando))
    expect(Cursando.estado).to eq  'cursando muy duro, como un esclavo'
  end
end




describe 'Punto 3' do

  it 'Probando el patron "Cualquier cosa"' do

    algo  = 'perro'

    retorno = case algo
      when _
        1
      else
        0
    end
    expect(retorno).to be 1
  end

  it 'Probando el "Pertenecer a un tipo"' do
    class GENTE

    end
    algo = GENTE.new

    retorno = case algo
      when is_a(GENTE)
        1
      else
        0
    end
    expect(retorno).to be 1
  end

  it 'Probando el "tener cierto valor en un atributo"' do
    case_class Pepe do
      attr_accessor :nombre, :edad
    end

    pepe = Pepe('Pepe', 100)

    retorno = case pepe
      when has(:nombre, 'Raul')
        -1
      when has(:apellido, 'Gonzales')
       0
      when has(:edad, 100)
        1
    end
    expect(retorno).to be 1
  end

  it 'Probando la "Comparacion estructural"' do

    case_object Vagueando do
    end

    case_class Laburando do
      attr_accessor :horas
    end

    case_class Persona do
      attr_accessor :nombre, :estado
    end

    pepe = Persona('Pepe', Vagueando)
    gercho = Persona('German', Laburando(9))

    expect(Persona(_,has(:horas,9)) === gercho).to be true
    expect(Persona(_, Vagueando) === pepe).to be true
    expect(Persona('German', Vagueando) === gercho).to be false
    expect(Persona('Pepe',has(:horas,9)) === pepe).to be false

  end

end