# -*- coding: utf-8 -*-
class CodeWriter


  def initialize(out)
    @prepend = ""
    @prependInc = "    "
    @line = ""
    @out = out
  end

  def CodeWriter.into_file(file_name)
    o = File.new(file_name, "w")
    return CodeWriter.new(o)
  end

  def print(data)
    @line += " " if @line.size > 0
    @line += data.to_s

    #if @line.size > 80
    #  line
    #  @line += "      "
    #end
  end

  def println(data)
    print(data)
    line
  end

  def header(filename)
    copyright =  "/**************************************************************************"
    copyright += "\n**\n** \tCopyright 2010 Duke Inc.\n"
    copyright += "** \tAttention, this file is automatically generated.\n"
    copyright += "** \tPlease do not modified it by manual.\n**\n"
    copyright += "**************************************************************************/"
    copyright += "\n\n"
    ifndef = ""
    if not filename.empty?
      ifndef += "#ifndef _" + filename.upcase() + "_H_\n"
      ifndef += "#define _" + filename.upcase() + "_H_"
      ifndef += "\n\n"
    end
    @out.puts copyright + ifndef + @line
    @line = ""
    @out.flush
  end

  def tail(filename)
    endif = ""
    if not filename.empty?
      endif = "#endif /* _" + filename.upcase() + "_H_ */\n"
    end
    @out.puts endif + @line
    @line = ""
    @out.flush
  end

  def line
    @out.puts @prepend + @line
    @line = ""
    @out.flush
  end

  def indent
    if @line.size > 0
      line
    end
    @prepend += @prependInc
  end

  def dedent
    if @line.size > 0
      line
    end
    if @prepend.size >= @prependInc.size
      @prepend = @prepend[@prependInc.size, @prepend.length]
    else
      @prepend = ""
    end
  end

  def clear_indentation
    if @line.size > 0
      line
    end
    @prepend = ""
  end

  def close
    @out.close
  end
end

class XElement

  def initialize(tag)
    @tag = tag
    @attributes = {}
    @subs = []
  end

  def add_attr(name, value)
    @attributes[name] = value
  end

  def add(element)
    @subs << element
  end

  def new_element(tag)
    s = XElement.new(tag)
    @subs << s
    return s
  end

  def dumpit(w)
    w.print("<" + @tag)

    @attributes.each do |k, v|
      w.print(" " + k + "=\"" + v + "\"")
    end

    if @subs.length == 0
      w.print("/>")
    else
      w.println(">")
      w.indent
      @subs.each do |s|
        if s.respond_to?("dumpit")
          s.dumpit(w)
        else
          w.println(s)
        end
      end
      w.dedent
      w.println("</" + @tag + ">")
    end
  end
end

class SimplePage
  attr_reader :top, :head, :body

  def initialize(title)
    @top = XElement.new("html")
    @head = @top.new_element("head")
    @body = @top.new_element("body")

    @head.new_element("title").add(title)
  end

  def add_css(url)
    l = @head.new_element("link")
    l.add_attr("rel", "stylesheet")
    l.add_attr("type", "text/css")
    l.add_attr("href", "style.css")
  end

  def dumpit(writer)
    if writer.is_a?(String)
      w = CodeWriter.new(File.new(writer, "w"))
    else
      w = writer
    end
    @top.dumpit(w)
    if writer.is_a?(String)
      w.close()
    end
  end
end

class Section
  attr_reader :name, :subs
  attr_accessor :title

  def initialize(name, title)
    @name = name
    @title = title
    @subs = []
  end

  def add_unit(unit)
    @subs << unit
  end

  def dumpit(w)
    if subs.empty?
      return
    end

    w.print(@title)
    w.println(":")
    w.indent

    subs.each do |s|
      if s.respond_to?('dumpit')
        s.dumpit(w)
      else
        w.println(s)
      end
    end
    w.dedent
    w.line
  end
end

class CodeUnit
# attr_reader :subs, :sections
  attr_reader :subs
  attr_accessor :head, :tail, :link, :hasCode, :custom_code

  def initialize()
    @head = ""
    @tail = ""
    @link = ""
    @subs = []
    @sections = {}
    @hasCode = false
    @custom_code = []
  end

  def add_unit(unit)
    @subs << unit
  end

  def add_custom_code(code)
    @custom_code << code
  end

  def add_sectin(name, title)
    @sections[name] = Section.new(name, title)
  end

  def add_unit_to_sectin(name, unit)
    if @sections.has_key? name
      @sections[name].add_unit(unit)
    else
      @subs << unit
    end
  end

  def dump_all(w)
    if @link != ""
      w.print(@head)
      first = true
      subs.each do |s|
        s.dumpit(w)
        w.print(@link) if !first
        first = false
      end
      return
    end

    if subs.length == 0 && @sections.length == 0 && (not @hasCode) && @custom_code.length == 0
      w.println( @head + ";")
      return
    end

    w.println(@head)
    w.println("{")

    if not @sections.empty?
      @sections.values.each do |s|
        s.dumpit(w)
      end
      w.line
    end

    if not subs.empty?
      w.indent
      subs.each do |s|
        if s.respond_to?('dumpit')
          s.dumpit(w)
        else
          w.println(s)
        end
      end
      w.dedent
      w.line
    end

    if not @custom_code.empty?
      @custom_code.each do |c|
        w.println(c)
      end
    end

    w.print("};")
    w.println(@tail)
  end
end

class ClassGen < CodeUnit
  attr_reader :name, :access, :supers

  def initialize(classtype, access, name)
    super()
    @classtype = classtype
    @name = name
    @access = access
    @supers = []
  end

  def add_super(sup)
    @supers << sup
  end

  def dumpit(w)
    decl = @classtype + " " + name
    if @supers.length > 0
      decl += " : " + access + " " + @supers.join(", ")
    end
    @head = decl

    dump_all(w)
  end
end

class EnumGen < CodeUnit
  attr_reader :name, :types

  def initialize(name)
    super()
    @name = name
    @types = []
  end

  def add_type(type)
    @types << type
  end

  def dumpit(w)
    w.println("enum " + name);
    w.println("{");
    w.indent;
    types.each {
      |t|
      w.println(t + ",")
    }
    w.dedent;
    w.println("};");
  end
end

class MethodGen < CodeUnit
  attr_reader :name, :return_type, :initializers, :parameters, :is_const

  def initialize(return_type, name, is_const = false)
    super()
    @name = name
    @return_type = return_type
    @initializers = []
    @parameters = []
    @is_const = is_const
  end

  def add_initializer(ini)
    @initializers << ini
  end

  def add_parameter(para)
    @parameters << para
  end

  def dumpit(w)
    if return_type.empty?
      decl = name
    else
      decl = return_type + " " + name
    end

    params = "(" + @parameters.join(", ") + ")"
    decl += params
    decl += ": " + @initializers.join(", ") if @initializers.length > 0
    decl += " const" if is_const
    self.head = decl
    dump_all(w)
  end
end

class VarGen < CodeUnit
  attr_reader :var_names, :type, :is_array, :qualifier, :init_code

  def initialize(qualifier, type, name, is_array, init_code)
    super()
    @qualifier = qualifier
    @type = type
    @var_name = name
    @is_array = is_array
    @init_code = init_code
  end

  def dumpit(w)
    if not @qualifier.empty?
      defination = @qualifier + " "
    else
      defination = ""
    end

    if @is_array
      defination = defination + "std::vector< " + type + " > " + @var_name
    else
      defination = defination + type + " " + @var_name
    end

    if not @init_code.empty?
      defination = defination + " = " + @init_code
    end

    defination = defination + ";"

    w.println(defination)
  end
end

class CodeCompare

  def initialize(file, tempfile)
    @file = file
    @tempfile = tempfile
  end

  def is_diff
    if not File.exists?(@file)
      return true
    end

    max_lines = 9999999
    str = %x(diff --unified=#{max_lines} #{@file} #{@tempfile})
    return (not str.empty?)
  end

  def replace_if_diff    
    if is_diff()
      if File.exists?(@file)
        File.delete(@file)
      end
      File.rename(@tempfile, @file)
      # use stderr, not to be redirected
      $stderr.puts "===> Replace " +  @file
    else
      File.delete(@tempfile)
    end
  end

end

if __FILE__ == $0
  # o = CodeWriter.into_file("test")

  ss = "xyz"
  s1 = ss + ss;
  s2 = s1[ss.length - 1, s1.length]

  puts ss;
  puts s1;
  puts s2;

  o = CodeWriter.new($stdout)
  o.print("a")
  o.line
  o.print("bb")
  o.indent
  o.indent
  o.println("line")
  o.print(13.75)
  o.print("!=")
  o.print(25)
  o.dedent
  o.print("it was")
  o.print("xc                                    cx")
  o.print("xc                                    cx")
  o.print("xc                                    cx")
  o.clear_indentation
  o.println("back")
end

# ex:set ts=2 sw=2 et:
