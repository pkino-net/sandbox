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
