require 'rexml/document'
include REXML


$built_literal_type = ["bool", "int", "std::vector<char>", "std::string"]
$built_id_type = ["center_id_t", "nb_id_t", "container_id_t", "storage_id_t", "anchor_id_t", "root_committer_id_t", "center_committer_id_t", "host_id_t", "host_committer_id_t", "transaction_id_t", "execution_id_t", "bridge_id_t", "bridge_factory_id_t", "facade_id_t", "container_id_t", "access_id_t"]


class XMLData
  attr_reader :errors, :name, :element_type

  def initialize(name, type)
    @errors = []
    @name = name
    @element_type = type

    # puts "creating unit " + type + ": " + name
  end

  def add_error(error)
    @errors << error
  end

  def print_error(path)
    if @errors.length > 0
      puts "----------------------- reporting from " + @name + ", a " + @element_type
      @errors.each {
        |e|
        puts path + " ==> " + e
      }
    end
  end

  def print_errors(path, sub)
    sub.each {
      |s|
      s.print_error(path + "/" + s.name + "(" + s.element_type + ")")
    }
  end

  def checkup(core)
  end

  def check_them(core, collection)
    collection.each {
      |d|
      d.checkup(core)
    }
  end
end

class Core < XMLData
  attr_reader :actors, :types


  def initialize(dir_name)
    super("core", "core")
    @files = []
    @actors = {
    }
    @types = {
    }

    if not File.exist?(dir_name)
      puts "file does not exist: " + dir_name
      return
    end

    if File.directory?(dir_name)
      Dir.foreach(dir_name) {
        |f|
        if f.end_with?(".xml")
          @files << XMLFile.new(dir_name + "/" + f)
        end
      }
    elsif dir_name.end_with?(".xml")
      @files << XMLFile.new(dir_name)
    else
      puts "file is not xml: " + dir_name
      return
    end

    checkup
  end

  def checkup
    @files.each {
      |f|
      add_them("actor", @actors, f.actors)
      add_them("type", @types, f.types)
      f.external_types.each {
        |n|
        if @types[n] == nil
          add_error("Undefined external type from " + f.name + ": " + n)
        end
      }
    }
    check_them(self, @actors.values)
    check_them(self, @types.values)
  end

  def add_them(label, collection, data)
    data.each {
      |d|
      n = d.name
      if collection[n] != nil
        add_error("Repeated definition for " + label + ": " + n)
      end
      collection[n] = d
    }
  end

  def print_error(path)
    super(path)
    print_errors(path, @types.values)
    print_errors(path, @actors.values)
  end

  def is_built_type(type)
    if $built_literal_type.include?(type) or $built_id_type.include?(type)
      return true;
    end
    return false;
  end

end

class XMLFile < XMLData

  attr_reader :actors, :types, :external_types

  def initialize(file)
    super(file, "file")
    @actors = []
    @types = []
    @external_types = []

    root = Document.new(File.new(file)).root
    ns = root.attributes["namespace"]

    root.elements.each {
      |e|
      if e.expanded_name == "type"
        @types << Type.new(ns, e)
      elsif e.expanded_name == "actor"
        @actors << Actor.new(ns, e)
      elsif e.expanded_name == "external_type"
        @external_types << e.attribute("name").to_s
      end
    }
  end

end

class Type < XMLData
  attr_reader :namespace, :fields

  def initialize(ns, xml)
    super(xml.attribute("name").to_s, "type")
    @namespace = ns
    @fields = {
    }

    xml.elements.each {
      |e|
      f = Field.new(e.expanded_name, name, e)
      name = f.name
      if @fields[name] != nil
        @errors << "repeated field name in " + super.name + ": " + name
      end
      @fields[name] = f
    }
  end

  def actors
    res = []
    fields.each {
      |key, value|
      t = value.actor
      if t != nil
        res << t
      end
    }
    return res
  end

  def subtypes
    res = []
    fields.each {
      |key, value|
      t = value.subtype
      if t != nil
        res << t
      end
    }
    return res
  end

  def checkup(core)
    check_them(core, @fields.values)
  end

  def print_error(path)
    super(path)
    print_errors(path, @fields.values)
  end
end

class Field < XMLData

  attr_reader :type_name, :tag_name, :actor_name, :is_array

  def initialize(tag, name, xml)
    super(xml.attribute("name").to_s, "field")
    @tag_name = tag
    @is_array = xml.attribute("is_array").to_s == "true"

    if tag == "id"
      @type_name = xml.attribute("id_type").to_s
      @actor_name = xml.attribute("actor_type").to_s
    else
      @type_name = xml.attribute("type").to_s
      @actor_name = ""
    end
  end

  #def actor
  #  if @tag_name != "reference"
  #    return nil
  #  end
  #  return @type_name
  #end

  #def subtype
  #  if @tag_name != "sub"
  #    return nil
  #  end
  #  return @type_name
  #end

  def checkup(core)
    if @tag_name == "sub" && core.types[@type_name] == nil
      add_error("Type name for sub-structure not found: " + @type_name)
    elsif @tag_name == "id" && core.actors[@actor_name] == nil
      add_error("Cannot use unknow actor: " + @type_name)
    elsif @tag_name == "id" && core.actors[@actor_name].is_singleton
      add_error("Cannot use id of singleton: " + @type_name)
    end
  end
end

class Actor < XMLData
  attr_reader :namespace, :calls, :is_singleton, :include, :custom_code, :inits, :dest
  def initialize(ns, xml)
    super(xml.attribute("name").to_s, "actor")
    @namespace = ns
    @is_singleton = xml.attribute("is_singleton").to_s == "true"
    @inits = []
    @calls = {
    }
    @include = []
    @custom_code = []

    if @is_singleton
      puts self.name + " is singleton"
    end
    xml.elements.each {
      |e|
      if e.expanded_name == "call"
        ename = e.attribute("name").to_s
        if @calls[ename] != nil
          errors << "repeated call name: " + ename
        end
        @calls[ename] = Call.new(@name, ename, e, @is_singleton)
      elsif e.expanded_name == "init"
        @inits << Init.new(@name, e, @is_singleton)
      elsif e.expanded_name == "dest"
        @dest = Dest.new(@name, e, @is_singleton)
      elsif e.expanded_name == "include"
        @include << e.get_text
      elsif e.expanded_name == "custom_code"
        @custom_code << e.get_text
      end
    }
  end

  def call(name)
    return calls[name]
  end

  def print_error(path)
    super(path)
    print_errors(path, @calls.values)
  end

  def checkup(core)
    return check_them(core, @calls.values)
  end
end

class Init < XMLData
  attr_reader :actor_name, :name, :input_type, :async, :pseudo_codes, :call_outs, :actor_is_singleton

  def initialize(actor, xml, is_singleton)
    super(name, "init")
    @name = "init"
    @actor_name = actor
    @actor_is_singleton = is_singleton
    @input_type = xml.attribute("input_type").to_s
    @async = xml.attribute("async").to_s == "true"
    @pseudo_codes = []
    @call_outs = []

    xml.elements.each {
      |e|
      if e.expanded_name == "pseudo_code"
        @pseudo_codes << PseudoCode.new(e)
      end
    }

    xml.elements.each {
      |e|
      if e.expanded_name == "call_out"
        @call_outs << CallOut.new(e)
      end
    }
  end

  def print_error(path)
    super(path)
    print_errors(path, @call_outs)
  end

  def checkup(core)
    check_them(core, @call_outs)
    if not self.async
      @call_outs.each {
        |o|
        if o.async and (not o.remote_decoupled)
          add_error("can not call async from a sync call: " + o.target_type + "/" + o.call_name)
        end
      }
    end

    if @is_singleton and (not @input_type.empty?)
      add_error("singleton initialization could not support input type: " + @actor_name)
    end
  end
end

class Dest < XMLData
  attr_reader :actor_name, :async, :pseudo_codes, :call_outs, :actor_is_singleton

  def initialize(actor, xml, is_singleton)
    super(name, "dest")
    @actor_name = actor
    @actor_is_singleton = is_singleton
    @async = xml.attribute("async").to_s == "true"
    @pseudo_codes = []
    @call_outs = []

    xml.elements.each {
      |e|
      if e.expanded_name == "pseudo_code"
        @pseudo_codes << PseudoCode.new(e)
      end
    }

    xml.elements.each {
      |e|
      if e.expanded_name == "call_out"
        @call_outs << CallOut.new(e)
      end
    }
  end

  def print_error(path)
    super(path)
    print_errors(path, @call_outs)
  end

  def checkup(core)
    check_them(core, @call_outs)
    if not self.async
      @call_outs.each {
        |o|
        if o.async and (not o.remote_decoupled)
          add_error("can not call async from a sync call: " + o.target_type + "/" + o.call_name)
        end
      }
    end
  end
end

class Call < XMLData
  attr_reader :actor_name, :input_type, :output_type, :async, :remoteable, :short_response, :pseudo_codes, :call_outs, :actor_is_singleton

  def initialize(actor, name, xml, is_singleton)
    super(name, "call")
    @actor_name = actor
    @actor_is_singleton = is_singleton
    @input_type = xml.attribute("input_type").to_s
    @output_type = xml.attribute("output_type").to_s
    @async = xml.attribute("async").to_s == "true"
    @remoteable = xml.attribute("remoteable").to_s == "true"
    @short_response = xml.attribute("short_response").to_s == "true"
    @pseudo_codes = []
    @call_outs = []

    if @input_type == "boolean"
      @input_type = "bool"
    elsif @input_type == "integer"
      @input_type = "int"
    elsif @input_type == "byte_array"
      @input_type = "std::vector<char>"
    else
    end

    if @output_type == "boolean"
      @output_type = "bool"
    elsif @output_type == "integer"
      @output_type = "int"
    elsif @output_type == "byte_array"
      @output_type = "std::vector<char>"
    else
    end

    xml.elements.each {
      |e|
      if e.expanded_name == "pseudo_code"
        @pseudo_codes << PseudoCode.new(e)
      end
    }

    xml.elements.each {
      |e|
      if e.expanded_name == "call_out"
        @call_outs << CallOut.new(e)
      end
    }
  end

  def print_error(path)
    super(path)
    print_errors(path, @call_outs)
  end

  def is_built_type(type)
    if $built_literal_type.include?(type) or $built_id_type.include?(type)
      return true;
    end
    return false;
  end

  def checkup(core)
    if @input_type != "" && core.types[@input_type] == nil
      if not is_built_type(@input_type)
        add_error("input type missing: " + @input_type)
      end
    end
    if @output_type != "" && core.types[@output_type] == nil
      if not is_built_type(@output_type)
        add_error("output type missing: " + @output_type)
      end
    end
    check_them(core, @call_outs)
    if not self.async
      @call_outs.each {
        |o|
        if o.async and (not o.remote_decoupled)
          add_error("can not call async from a sync call: " + o.target_type + "/" + o.call_name)
        end
      }
    end
    if self.async and (self.output_type == "")
      add_error("async call without the output : " + self.name)
    end


    checkup_caller(core)
  end

  def checkup_caller(core)
    isfind = false
    core.actors.each {
      |k1, a|
      a.calls.each {
          |k2, c|
          c.call_outs.each {
            |co|
            if co.target_type == @actor_name && co.call_name == @name
              isfind = true
          end
          }
      }

      a.inits.each {
        |c|
        c.call_outs.each {
          |co|
          if co.target_type == @actor_name && co.call_name == @name
            isfind = true
          end
        }
      }
    }

    if not isfind
      add_error("Caller missing: " + @actor_name + "(" + @name + ")")
    end
  end

  def reproduce()
    call =  XElement.new("call")
    call.add_attr("name", @name)
    if not @input_type.empty?
      call.add_attr("input_type", @input_type)
    end
    if not @output_type.empty?
    call.add_attr("output_type", @output_type)
    end
    call.add_attr("async", @async)
    call.add_attr("short_response", @short_response)
    @call_outs.each {
      |co|
      call.add(co.reproduce())
    }
    return call
  end

end

class CallOut < XMLData
  attr_reader :target_type, :call_name, :remote_decoupled, :async

  def initialize(xml)
    super(xml.attribute("call_name").to_s, "call_out")
    @target_type = xml.attribute("target_type").to_s
    @call_name = self.name
    @remote_decoupled = xml.attribute("remote_decoupled").to_s == "true"
    @async = false
  end

  def checkup(core)
    actors = core.actors
    a = actors[@target_type]
    if a == nil
      add_error("No actor found for call " + @target_type)
    elsif a.call(@call_name) == nil
      add_error("No call found in actor " + @target_type + ": " + @call_name)
    elsif
      @async = a.call(@call_name).async
    end
  end

  def reproduce()
    call_out =  XElement.new("call_out")
    call_out.add_attr("target_type", @target_type)
    call_out.add_attr("call_name", @call_name)
    return call_out
  end

end

class PseudoCode < XMLData
  attr_reader :pseudo_code

  def initialize(xml)
    @pseudo_code = xml.get_text
  end
end

if __FILE__ == $0
  o = Core.new("./res")
  o.checkup
  o.print_error("core")
end
