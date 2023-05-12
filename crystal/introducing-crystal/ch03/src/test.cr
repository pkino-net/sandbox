class Foo
  def to_s
    "foo"
  end
end

class Bar
  def to_s
    "bar"
  end
end

def to_s(x)
  puts(typeof(x))
  x.to_s
end

to_s(Bar.new)

to_s(Foo.new)
