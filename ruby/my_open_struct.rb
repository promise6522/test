require 'delegate'

class MyOpenStruct
  def initialize
    @attributes = {}
  end

  def greeting
    puts "MyOpenStruct::greeting"
  end

  def method_missing(name, *args)
    attribute = name.to_s
    if attribute =~ /=$/
      @attributes[attribute.chop] = args[0]
    else
      @attributes[attribute]
    end
  end
end

icecream = MyOpenStruct.new
wrapper = SimpleDelegator.new(icecream)

wrapper.greeting

# the following would fail, for the implementation of SimpleDelegator
# before sending the message, it checks if the delegate_obj respond to the msg
# otherwise send to msg to default method_missing, which raise and exception
wrapper.flavor = "vanilla"
puts wrapper.flavor


      
