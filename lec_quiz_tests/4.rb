module Animal
    def greet
       "Hi Friend"
    end
  end
  
  module Person
    def greet
      "Hello"
    end
      
  end
   
  class Prokaryote
    include Animal
  end
  
  class Organism < Prokaryote
      include Person
      
      def meet
        puts greet
      end
  end
   
  being = Organism.new
  being.meet