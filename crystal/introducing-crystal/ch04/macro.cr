macro my_macro(method_name, content)
  def {{method_name}}
    {{content}}
  end
end

my_macro(my_method, "hoge")

macro my_getter(*names)
  {% for name in names %}
    def {{name.id}}
      @{{name}}
    end
  {% end %}
end

puts(my_method)

class User
  def initialize(@name : String, @age : Int32)
  end

  my_getter name, age
end

user = User.new("Taro", 30)
puts(user.name)
puts(user.age)

class Dog
  def initialize(@name : String, @age : Int32)
  end

  getter name, age
end

dog = Dog.new("Jiro", 5)
puts(dog.name)
puts(dog.age)

struct Book
  def initialize(@title : String, @edition : Int32)
  end
end

struct EqBook
  def initialize(@title : String, @edition : Int32)
  end

  def_equals @title
end

puts(Book.new("すごい本", 1) == Book.new("すごい本", 1))
puts(Book.new("すごい本", 1) == Book.new("すごい本", 2))
puts(EqBook.new("すごい本", 1) == EqBook.new("すごい本", 2))

record Human, name : String, age : Int32

puts(Human.new "Taro", 30)
