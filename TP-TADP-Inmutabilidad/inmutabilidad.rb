class Object
  def self.const_missing nombre
    objeto = Object.new
    objeto.instance_eval do
      @nombre = nombre
      def nombre
        @nombre
      end
      def padre
        @padre
      end
      def < superclase
        if (superclase.is_a? Class)
          @padre = superclase
          self
        else
          raise TypeError, 'superclass must be a Class'
        end
      end
    end
    objeto
  end
end




module Inmutabilidad

  @@bloque_case_class = proc {

  #CONSTRUCTOR FACHERO
  clase = self
  Inmutabilidad.send(:define_method, clase.to_s, proc {|*argumentos| clase.new(*argumentos)})


  #NO HACE FALTA DEFINIR EL INITIALIZE
  def self.attr_accessor(*args)
    self.class_variable_set(:@@propiedades, args)
    def initialize(*valores)
      self.class.class_variable_get(:@@propiedades).zip(valores).each do |argumento,valor|
        instance_variable_set("@#{argumento}", valor)
      end
    end
    attr_reader(*args)
  end


  #BLOQUEO LA HERENCIA
  def self.inherited(subclase)
    Object.send(:remove_const,subclase.to_s)
    raise NotImplementedError,'No se puede heredar de una Case Class'
  end


  #INSTANCIAS INMUTABLES
  def self.new *args
    super(*args).freeze
  end


  #REDEFINO EL ==
  def == (instancia)
    if metodo_definido_en_alguna_superclase?(:==)
      super(instancia)
    else
      self.class == instancia.class && self.atributos == instancia.atributos
    end
  end


  #REDEFINO EL TO_S
  def to_s
    if metodo_definido_en_alguna_superclase?(:to_s)
      super
    else
      self.class.to_s + "(#{atributos.map {|atr| atr.to_s}.join(', ').gsub(/\\"/, '')})"
    end
  end


  #REDEFINO EL HASH
  def hash
    if metodo_definido_en_alguna_superclase?(:hash)
      super
    else
      7 + atributos.map {|atributo| atributo.hash}.reduce(0, :+)
    end
  end


  #REDEFINO EL COPY
  def copy(*lambdas)
    valores_nuevos = []
    instance_variables.each do |atributo|
      valor_viejo = instance_variable_get atributo
      lambda = lambdas.select {|lambda| primer_parametro_de(lambda) == nombre_propiedad_de(atributo)}.first
      if !lambda.nil?
        valor_nuevo = lambda.call(valor_viejo)
      else
        valor_nuevo = valor_viejo
      end
      valores_nuevos.push(valor_nuevo)
    end
    self.class.new(*valores_nuevos)
  end


  #AUXILIARES
  def nombre_propiedad_de(atributo)
    atributo.to_s[1, atributo.to_s.length - 1]
  end

  def primer_parametro_de(lambda)
    lambda.parameters.first.last.to_s
  end

  def superclases
    self.class.ancestors[1.. self.class.ancestors.index(Object)-1]
  end

  def metodo_definido_en_alguna_superclase?(metodo)
    superclases.any? {|superclase| superclase.instance_methods(false).include?(metodo)}
  end

  def atributos
    self.instance_variables.map {|variable| instance_variable_get variable}
  end


  #PATTERN MATCHING
  def ===(algo)
    algo.class == self.class && self.atributos.zip(algo.atributos).all? {|mio, ajeno| mio === ajeno}
  end
  }

  @@bloque_case_object = proc {
    def to_s
      self
    end

    def self.attr_accessor(*args)
      raise NoMethodError
    end
  }



  private def crear_clase(firma)
    nombre_superclase = firma.padre
    nombre_clase = firma.nombre

    if !nombre_superclase.nil?
      clase = Class.new nombre_superclase
    else
      clase = Class.new
    end
    Object.const_set(nombre_clase, clase)
    clase
  end

  private def crear_objeto(firma)
    objeto = crear_clase(firma).new
    Object.const_set(firma.nombre, objeto)
    objeto
  end



  def case_class(firma, &bloque)
    clase = crear_clase(firma)
    clase.class_eval &@@bloque_case_class
    clase.class_eval &bloque
  end

  def case_object(firma, &bloque)
    objeto = crear_objeto(firma)
    objeto.instance_eval &@@bloque_case_object
    objeto.instance_eval &bloque

  end

end




module Pattern_Matching

  def _
    -> (_) {true}
  end

  def is_a tipo
    -> (x) {x.is_a? tipo}
  end

  def has(nombre_Atributo, valor)
    -> (x) {
      if x.class.instance_methods(false).include? nombre_Atributo
        x.send(nombre_Atributo) == valor
      else
        false
      end
    }
  end

end
