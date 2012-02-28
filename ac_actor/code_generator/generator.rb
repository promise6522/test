require "./src/spec_data.rb"
require "./src/code_writer.rb"

$actor_id_map = { "ac_root_committer" => "root_committer_id_t",
                  "ac_center_committer" => "center_committer_id_t",
                  "ac_host_committer" => "host_committer_id_t",
                  "ac_transaction" => "transaction_id_t",
                  "ac_access" => "access_id_t",
                  "ac_execution" => "execution_id_t",
                  "ac_bridge_factory" => "bridge_factory_id_t",
                  "ac_container" => "container_id_t",
                  "ac_anchor" => "anchor_id_t",
                  "ac_object" => "nb_id_t",
                  "ac_storage_facade" => "facade_id_t",
                  "ac_storage" => "storage_id_t"
                }


#=============================================================
#                    Actor Type Generator
#=============================================================
class ActorTypeWriter
  attr_reader :core, :actor_type

  def initialize(core)
    @core = core
    @actors = []
    @methods = []
    @funs = []
    @singleton_acid = []
    write_actor_type
    write_actor_method
    write_actor_factory
    write_actor_destory
  end

  def write_actor_factory
    @core.actors.each {
      |k, a|
      if not a.is_singleton
        fun = "bool actor_factory(" + "const " + $actor_id_map[a.name] + "& id, ac_actor*& pActor);"
        @funs << fun

        a.inits.each {
          |i|
          if not i.input_type.empty?
            fun = "bool actor_factory(" + "const " + $actor_id_map[a.name] + "& id,  ac_actor*& pActor, const " + i.input_type + "& data);"
            @funs << fun
          end
        }
      end
    }
  end

  def write_actor_destory
    fun = "void actor_destory(ac_actor* pActor);"
    #fun += "{\n"
    #fun += "    pActor->destruction();\n"
    #fun += "    ac_memory_alloctor<Ta>::instance().deallocate(pActor);\n}"
    @funs << fun
  end

  def write_actor_type
    @actor_type = EnumGen.new("ac_actor_t")
    @actor_type.add_type("e_ac_framework")
    @actors << "ac_framework"
    @singleton_acid << "ac_framework"

    sorted_actors = []
    @core.actors.each {
      |k, a|
      sorted_actors << a.name
      if a.is_singleton
        @singleton_acid << a.name
      end
    }
    sorted_actors.sort!
    sorted_actors.each { |e|
      @actor_type.add_type("e_" + e)
      @actors << e
    }

    @actor_type.add_type("e_ac_actor_size")
  end

  def write_actor_method
    @actor_method = EnumGen.new("ac_method_t")
    @actor_method.add_type("e_ac_actor_exception")
    @methods << "ac_actor_exception"
    @actor_method.add_type("e_ac_actor_exit")
    @methods << "ac_actor_exit"

    # sort the actor methods before dump to enum
    sorted_methods = []
    @core.actors.values.each { |a|
      a.calls.values.each { |c|
        if c.async
          sorted_methods << "#{a.name}_#{c.name}"
        end
      }
    }
    sorted_methods.sort!
    sorted_methods.each { |m| 
      @actor_method.add_type("e_#{m}")
      @actor_method.add_type("e_#{m}_response")
      @methods << m
      @methods << m + "_response"
    }

    @actor_method.add_type("e_ac_method_size")

  end

  def dumpit(w, filename)
    w.header(filename)
    w.line
    w.println("#include <string>")
    w.println("#include <map>")
    w.println("#include \"ac_global.h\"")
    w.println("#include \"ac_memory_alloctor.h\"")
    w.println("#include \"ac_message_type.h\"")
    w.line
    w.println("class ac_actor;")
    w.line
    # actor types
    @actor_type.dumpit(w)
    w.line
    # actor type string
    w.println("const std::string ac_actor_string [] =")
    str = "{\n"
    @actors.each {
      |a|
      str = str + "    \"" + a + "\", \n"
    }
    str = str + "};"
    w.println(str)
    w.line
    # singleton actor list
    w.println("const ac_actor_t ac_singleton_actor [] =")
    str = "{\n"
    @singleton_acid.each {
      |a|
      str = str + "    e_" + a + ",\n"
    }
    str = str + "};"
    w.println(str)
    w.line
    # actor method
    @actor_method.dumpit(w)
    w.line
    # actor method string
    w.println("const std::string ac_method_string [] =")
    str = "{\n"
    @methods.each {
      |m|
      str = str + "    \"" + m + "\", \n"
    }
    str = str + "};"
    w.println(str)
    w.line
    @funs.each {
      |m|
      w.println(m)
    }
    w.println("void init_singleton_actor();")
    w.line
    # singleton actor id
    @singleton_acid.each {
      |a|
      w.println("extern ac_id_t g_" + a + "_acid;")
    }
    w.println("extern ac_id_t* g_singleton_actors[" + @singleton_acid.size.to_s+ "];")
    w.line
    # write actor declaration
    @actors.each {
      |a|
      w.println("class " + a + ";")
    }
    w.tail(filename)
  end
end

#=============================================================
#                    Remote Message Helper Generator
#=============================================================
class RemoteMessageHelperWriter
  attr_reader :core, :methods  

  # A struct to notate a remote method
  RemoteMethod = Struct.new(:name, :input, :output)

  def initialize(core)
    @core = core
    @methods = []
    collect_remote_methods
  end

  def collect_remote_methods
    @core.actors.values.each {
      |a|
      a.calls.values.each {
        |c|
        if c.async and c.remoteable
          m = RemoteMethod.new
          m.name = "e_" + a.name + "_" + c.name
          m.input = c.input_type
          m.output = c.output_type
          @methods << m
        end
      }
    }
  end

  def gen_deserialize_case(w, method, param)
      w.println("case #{method}:")
      w.println("{")
      w.indent
      if param != ""
        w.println("#{param}* pData = ac_memory_alloctor<#{param}>::instance().allocate();")
        w.println("pData->unpack(data);")
        w.println("return pData;")
      else
        w.println("return NULL;")
      end
      w.dedent
      w.println("}")
  end

  def gen_serialize_case(w, method, param)
      w.println("case #{method}:")
      w.println("{")
      w.indent
      if param != ""
        w.println("#{param}* pOutput = reinterpret_cast<#{param}*>(pData);")
        w.println("data = pOutput->pack();")
        w.println("ac_memory_alloctor<#{param}>::instance().deallocate(pOutput);")
      else
        w.println("data = \"\";")
      end
      w.println("break;")
      w.dedent
      w.println("}")
  end

  def dumpit(w, filename)
    w.header(filename)
    w.println("#include <string>")
    w.println("#include \"ac_global.h\"")
    w.println("#include \"ac_memory_alloctor.h\"")
    w.println("#include \"ac_message_type.h\"")
    w.line

    # function for stream to pointer
    w.println("inline void* remote_message_deserialize(ac_method_t method, const std::string& data)")
    w.println("{")
    w.indent
    w.println("switch (method)")
    w.println("{")
    w.indent
    @methods.each { | m |
      gen_deserialize_case(w, m.name, m.input)
      gen_deserialize_case(w, m.name+"_response", m.output)
    }
    w.println("default:")
    w.println("    return NULL;")
    w.dedent
    w.println("}") # end of switch
    w.dedent
    w.println("}") # end of method

    w.line
    w.line

    # function for pointer to stream
    w.println("inline void serialize_to_remote_message(void* pData, ac_method_t method, std::string& data)")
    w.println("{")
    w.indent
    w.println("switch (method)")
    w.println("{")
    w.indent
    @methods.each { | m |
      gen_serialize_case(w, m.name, m.input)
      gen_serialize_case(w, m.name+"_response", m.output)
    }
    w.println("default:")
    w.println("    return;")
    w.dedent
    w.println("}") # end of switch
    w.dedent
    w.println("}") # end of method


    w.line
    w.line
    w.tail(filename)
  end
end


#=============================================================
#                    Actor Factory Generator
#=============================================================
class ActorFactoryWriter
  attr_reader :core, :methods, :vars

  def initialize(core)
    @core = core
    @methods = []
    @vars = []
    @singleton_acid = []
    collect_single_actor

    write_actor_destory
    write_actor_factory
    write_init_singleton
  end

  def collect_single_actor
    @singleton_acid << "ac_framework"
    @core.actors.each {
      |k, a|
      if a.is_singleton
        @singleton_acid << a.name
      end
    }
  end

  def write_init_singleton
    fun = MethodGen.new("void", "init_singleton_actor")
    #create ac_actor
    @core.actors.each {
      |k, a|
      if a.is_singleton
        fun.custom_code << "    " + a.name + "* p_" + a.name + " = new(std::nothrow) " + a.name + ";"
        fun.custom_code << "    g_" + a.name + "_acid = p_" + a.name + "->get_actor_id();"
        fun.custom_code << "\n"
      end
    }

    #call initialization function
    @core.actors.each {
      |k, a|
      if a.is_singleton
        fun.custom_code << "    if(p_" + a.name + ")"
        fun.custom_code << "    {"

        isAsyncInit = false
        a.inits.each {
          |i|
          if i.async
            isAsyncInit = true
          end
        }
        if not isAsyncInit
          fun.custom_code << "        p_" + a.name + "->set_initialized_status();"
        else
          fun.custom_code << "        p_" + a.name + "->set_initializing_status();"
        end

        fun.custom_code << "        p_" + a.name + "->initialization();"
        fun.custom_code << "    }\n\n"
      end
    }
    @methods << fun
  end

  def write_actor_destory
    fun = MethodGen.new("void", "actor_destory")
    fun.add_parameter("ac_actor* pActor")
    fun.custom_code << "    if(!pActor)"
    fun.custom_code << "    {"
    fun.custom_code << "        LOG_ERROR(\"Try to destory empty ac_actor pointer.\");"
    fun.custom_code << "        return;"
    fun.custom_code << "    }\n\n"
    fun.custom_code << "    pActor->destruction();\n\n"
    fun.custom_code << "    ac_actor_t ac_type = pActor->get_actor_type();\n\n"
    fun.custom_code << "    switch(ac_type)"
    fun.custom_code << "    {"

    @core.actors.each {
      |k, a|
      fun.custom_code << "        case e_" + a.name + " : "
      fun.custom_code << "        {"
      if not a.is_singleton
        fun.custom_code << "            " + a.name + " *ptr = static_cast<" + a.name + "*>(pActor);"
        fun.custom_code << "            ac_memory_alloctor<" + a.name + ">::instance().deallocate(ptr);"
      else
        fun.custom_code << "            delete pActor;"
      end
      fun.custom_code << "            break;"
      fun.custom_code << "        }"

    }

    fun.custom_code << "        default : "
    fun.custom_code << "        {"
    fun.custom_code << "            LOG_DEBUG(\"could not identify the ac_actor type.\");"
    fun.custom_code << "        }"
    fun.custom_code << "    }"
    @methods << fun
  end

  def write_actor_factory
    @core.actors.each {
      |k, a|
      if not a.is_singleton
        #write default constractor
        fun = MethodGen.new("bool", "actor_factory")
        fun.add_parameter("const " + $actor_id_map[a.name] + "& id")
        fun.add_parameter("ac_actor*& pAc")
        fun.custom_code << "    " + a.name + "* pActor = static_cast<" + a.name + "*>(pAc);"
        fun.custom_code << "    if(ac_manager::instance().create_actor(pActor, id))"
        fun.custom_code << "    {"
        fun.custom_code << "        pActor->set_id(id);"
        isAsyncInit = false
        a.inits.each {
          |i|
          if i.async and i.input_type.empty?
            isAsyncInit = true
          end
        }
        if not isAsyncInit
          fun.custom_code << "        pActor->set_initialized_status();"
        else
          fun.custom_code << "        pActor->set_initializing_status();"
        end

        if a.inits.empty?
          fun.custom_code << "        pActor->initialization();"
        else
          a.inits.each {
            |i|
            if i.input_type.empty?
              fun.custom_code << "        pActor->initialization();"
            end
            }
        end
        fun.custom_code << "        pAc = pActor;"
        fun.custom_code << "        return true;"
        fun.custom_code << "    }"
        fun.custom_code << "        pAc = pActor;"
        fun.custom_code << "    return false;"
        @methods << fun

        #write other constractor with input_type
        a.inits.each {
          |i|
          if not i.input_type.empty?
            fun = MethodGen.new("bool", "actor_factory")
            fun.add_parameter("const " + $actor_id_map[a.name] + "& id")
            fun.add_parameter("ac_actor*& pAc")
            fun.add_parameter("const " + i.input_type + "& data")
            fun.custom_code << "    " + a.name + "* pActor = static_cast<" + a.name + "*>(pAc);"
            fun.custom_code << "    if(ac_manager::instance().create_actor(pActor, id))"
            fun.custom_code << "    {"
            fun.custom_code << "        pActor->set_id(id);"
            if not i.async
              fun.custom_code << "        pActor->set_initialized_status();"
            else
              fun.custom_code << "        pActor->set_initializing_status();"
            end
            fun.custom_code << "        pActor->initialization(data);"
            fun.custom_code << "        pAc = pActor;;"
            fun.custom_code << "        return true;"
            fun.custom_code << "    }"
            fun.custom_code << "    pAc = pActor;"
            fun.custom_code << "    return false;"
            @methods << fun
          end
        }
      end
    }
  end

  def dumpit(w, filename)
    w.header("")
    w.println("#include \"ac_global.h\"")
    w.println("#include \"ac_actor_type.h\"")
    w.line
    @core.actors.each {
      |k, a|
      w.println("#include \"" + a.name + ".h\"")
    }
    w.line
    singleton_id_count = 0
    @singleton_acid.each {
      |a|
      w.println("ac_id_t g_" + a + "_acid = 0;")
    }
    w.line
    w.println("ac_id_t* g_singleton_actors[" + @singleton_acid.size.to_s+ "] = {")
    @singleton_acid.each {
      |a|
      w.println("    &g_" + a + "_acid,")
    }
    w.println("};")
    w.line
    @vars.each {
      |v|
      v.dumpit(w)
    }
    w.line
    @methods.each {
      |m|
      m.dumpit(w)
      w.line
    }
    w.line
    w.tail("")
  end
end

#=============================================================
#                    Message Type Generator
#=============================================================
class MessageTypeWriter
  attr_reader :core, :data, :actor_writer, :call_outs, :actor_type

  def initialize(core)
    @core = core
    @type_structs = []
    write_types
  end

  def write_types()
    type_order = []

    while 1

      if (@core.types.values.length == type_order.length)
        break
      end

      @core.types.values.each {
        |t|
        if type_order.include?(t.name)
          next
        end

        struct = ClassGen.new("struct", "public", t.name)
        is_fields_success = true
        msgpack_str = "MSGPACK_DEFINE("
        t.fields.each {
          |k, v|
          type_name = v.type_name
          var_name = v.name
          if v.tag_name == "literal"
            if v.type_name == "boolean"
              type_name = "bool"
            elsif v.type_name == "integer"
              type_name = "int"
            elsif v.type_name == "byte_array"
              type_name = "std::vector<unsigned char>"
            elsif v.type_name == "string"
              type_name = "std::string"
            elsif v.type_name == "local_pointer"
              ## when specify a local_pointer type
              ## the name field is actually the type of the C++ shared_ptr
              type_name = "std::tr1::shared_ptr<#{v.name}>"
              ## some tricks on the var name
              var_name = v.name + "_ptr"
            else
            end
          elsif v.tag_name == "id"
            type_name = v.type_name
          elsif v.tag_name == "sub"
            if (not type_order.include?(v.type_name))
              is_fields_success = false
              break
            else
            end
          else
          end

          var = VarGen.new("", type_name, var_name, v.is_array, "")
          struct.add_unit(var)

          if v.type_name != "local_pointer"
            msgpack_str = msgpack_str + var_name + ", "
          end
        } # end of fields

        msgpack_str = msgpack_str[0, msgpack_str.length - 2]
        msgpack_str = msgpack_str + ")"
        var = VarGen.new("", msgpack_str, "", false, "")
        struct.add_unit(var)
        
        #pack & unpack function 
        fun = MethodGen.new("std::string", "pack", true)
        fun.custom_code << "    return serialize_by_msgpack(*this);"
        struct.add_unit(fun)
      
        fun = MethodGen.new("void", "unpack")
        fun.add_parameter("const std::string& strval");
        fun.custom_code << "    deserialize_by_msgpack(strval, *this);"
        struct.add_unit(fun)
        
        if is_fields_success == true
          @type_structs << struct
          type_order << t.name
        end
      } # end of types

    end # end of while
  end

  def dumpit(w, filename)
    w.header(filename)
    w.println(%Q{#include <vector>})
    w.println(%Q{#include <string>})
    w.println(%Q{#include "ac_global.h"})
    w.println(%Q{#include "stdx_msgpack.h"})
    w.println(%Q{#include "ac_object/object_data.h"})
    w.line
    @type_structs.each {
      |s|
      s.dumpit(w)
      w.line
    }
    w.line
    w.tail(filename)
  end
end

#=============================================================
#                       Actor Generator
#=============================================================
class ActorWriter
  attr_reader :core, :data, :actor_writer, :call_outs

  def initialize(core, data)
    @core = core
    @data = data
    @actor_writer = ClassGen.new("class", "public", data.name)
    @call_outs = []
    @include = []
    collect

    @actor_writer.add_super("ac_actor")
    @actor_writer.add_sectin("constructor_methods", "public")
    @actor_writer.add_sectin("public_methods", "public")
    @actor_writer.add_sectin("private_methods", "private")
    @actor_writer.add_sectin("public_members", "public")
    @actor_writer.add_sectin("private_members", "private")

    write_include(data)
    write_constructor(data)
    write_custom_code(data)
    write_message_switch(data)
    write_set_id(data)
    write_exception_handle
    write_message_handle_over

    @data.inits.each {
      |i|
      write_init_call(i)
    }

    if @data.dest != nil
      write_dest_call()
    end

    @data.calls.values.each {
      |a|
      write_call_ins(a)
    }

    @call_outs.each {
      |a|
      write_call_outs(a)
    }

    write_members(data)
  end

  def collect
    @data.calls.values.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }

    @data.inits.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }
  end

  def add_call(out)
    @call_outs.each {
      |o|
      if out.target_type == o.actor_name && o.name == out.call_name
        return
      end
    }
    if @core.actors[out.target_type] == nil
      return
    end

    c = @core.actors[out.target_type].calls[out.call_name]
    if c != nil
      @call_outs << c
    end
  end

  def write_members(actor)
    me = VarGen.new("", actor.name + "_helper_ptr", "m_ptrHelper", false, "")
    @actor_writer.add_unit_to_sectin("private_members", me)
    if $actor_id_map[data.name] != nil
      me = VarGen.new("", $actor_id_map[data.name], "m_id", false, "")
      @actor_writer.add_unit_to_sectin("private_members", me)
    end
  end

  def write_constructor(actor)
    fun = MethodGen.new("", actor.name)
    fun.add_initializer("ac_actor(e_" + actor.name + ")")
    fun.add_initializer("m_ptrHelper(new(std::nothrow) "+ actor.name + "_helper(m_ac_id)) " )
    var = VarGen.new("", "", "m_actor_name", false, "\"" + actor.name + "\"")
    fun.add_unit(var)
    fun.hasCode = true
    @actor_writer.add_unit_to_sectin("constructor_methods", fun)
    fun = MethodGen.new("virtual", "~" + actor.name)
    fun.hasCode = true
    @actor_writer.add_unit_to_sectin("constructor_methods", fun)
  end

  def write_init_call(init)
    res = MethodGen.new("virtual bool", "initialization")
    if not init.input_type.empty?
      res.add_parameter("const " + init.input_type + "& data");
    end
    @actor_writer.add_unit_to_sectin("constructor_methods", res)
  end

  def write_dest_call()
    res = MethodGen.new("virtual bool", "destruction")
    @actor_writer.add_unit_to_sectin("constructor_methods", res)
  end

  def write_exception_handle
    exp = MethodGen.new("virtual bool", "exception_handle")
    exp.add_parameter("req_num_t req_num");
    exp.add_parameter("const std::string& str");
    @actor_writer.add_unit_to_sectin("private_methods", exp)
  end

  def write_set_id(data)
    if $actor_id_map[data.name] != nil
      fun = MethodGen.new("void", "set_id")
      fun.add_parameter("const " + $actor_id_map[data.name] + "&" + " id");
      fun.custom_code << "    m_id = id;\n"
      fun.custom_code << "    m_ptrHelper->set_id(id);\n"
      @actor_writer.add_unit_to_sectin("constructor_methods", fun)

      fun = MethodGen.new($actor_id_map[data.name], "get_id")
      fun.custom_code << "    return m_id;\n"
      @actor_writer.add_unit_to_sectin("constructor_methods", fun)

      fun = MethodGen.new("virtual nb_id_alias", "get_nb_id_alias")
      fun.custom_code << "    return m_id.to_nb_id_alias();\n"
      @actor_writer.add_unit_to_sectin("constructor_methods", fun)
    end
  end

  def write_message_handle_over()
      fun = MethodGen.new("virtual void", "message_handle_over")
      fun.custom_code << "    m_ptrHelper->commit_measurement_result();\n"
      @actor_writer.add_unit_to_sectin("constructor_methods", fun)
  end

  def write_call_ins(call)
    if not call.async
      req = MethodGen.new("bool", call.name)
      if not call.input_type.empty?
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      if not call.output_type.empty?
        req.add_parameter(call.output_type + "&" +" output");
      end
      @actor_writer.add_unit_to_sectin("public_methods", req)
    else
      req = MethodGen.new("bool", call.name)
      req.add_parameter("call_id_t call_id");
      if not call.input_type.empty?
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      @actor_writer.add_unit_to_sectin("private_methods", req)
    end
  end

  def write_call_outs(call)
    if call.async
      res = MethodGen.new("bool", call.actor_name + "_" + call.name + "_response")
      res.add_parameter("req_num_t req_num");
      if not call.output_type.empty?
        res.add_parameter(call.output_type + "&" + " output");
      end
      @actor_writer.add_unit_to_sectin("private_methods", res)
    end
  end

  def write_include(actor)
    @include = actor.include
  end

  def write_custom_code(actor)
    actor.custom_code.each {
      |c|
      @actor_writer.add_custom_code(c)
    }
  end

  def write_message_switch(actor)
    fun = MethodGen.new("virtual bool", "message_handle")
    fun.add_parameter("const ac_message_t& message");

    fun.custom_code << "    ac_actor::message_handle(message);\n"
    fun.custom_code << "    if(m_init_status == ac_init_fail)"
    fun.custom_code << "    {"
    fun.custom_code << "        if(!message.is_respond)"
    fun.custom_code << "        {"
    fun.custom_code << "            call_id_t call_id = {message.ac_id, message.req_num};"
    fun.custom_code << "            std::string exception_string "
    fun.custom_code << "                = \"ac_actor(\" + m_actor_name + \")[\" + get_nb_id_alias().str() + \"]is not init success\";"
    fun.custom_code << "            m_ptrHelper->exception_respond(call_id, exception_string);"
    fun.custom_code << "        }"
    fun.custom_code << "        LOG_ERROR(\"Send Msg to invalid actor(\"<<m_actor_name<<\")[\"<<get_nb_id_alias().str()"
    fun.custom_code << "                  <<\"] with status=\"<<m_status<<\"(\"<<static_cast<std::size_t>(m_init_status)<<\")\");"
    fun.custom_code << "        return true;"
    fun.custom_code << "    }"
    fun.custom_code << "    bool ret = false;"
    fun.custom_code << "    switch(message.type)"
    fun.custom_code << "    {"
    fun.custom_code << "        case e_ac_actor_exception : "
    fun.custom_code << "        {"
    fun.custom_code << "            std::string* pData = reinterpret_cast<std::string*>(message.data);"
    fun.custom_code << "            if(pData && (ac_status_exiting != m_status))"
    fun.custom_code << "            {"
    fun.custom_code << "                m_ptrHelper->start_measurement();"
    fun.custom_code << "                ret = exception_handle(message.req_num, *pData);"
    fun.custom_code << "                m_ptrHelper->stop_measurement(message.type);"
    fun.custom_code << "            }"
    fun.custom_code << "            ac_memory_alloctor<std::string>::instance().deallocate(pData);"
    fun.custom_code << "            break;"
    fun.custom_code << "        }"
    fun.custom_code << "        case e_ac_actor_exit : "
    fun.custom_code << "        {"
    fun.custom_code << "            call_id_t call_id = {message.ac_id, message.req_num};"
    fun.custom_code << "            ret = exit(call_id);"
    fun.custom_code << "            break;"
    fun.custom_code << "        }"

    # write all call
    actor.calls.values.each {
      |c|
      if c.async
        write_message_call_case(fun, c)
      end
    }

    # write all response
    @call_outs.each {
      |c|
      if c.async
        write_message_res_case(fun, c)
      end
    }

    fun.custom_code << "        default : "
    fun.custom_code << "        {"
    fun.custom_code << "            LOG_ERROR(\"the message type isn't support by this actor.\");"
    fun.custom_code << "            break;"
    fun.custom_code << "        }"
    fun.custom_code << "    }"
    fun.custom_code << "    return ret;"


    #var = VarGen.new("", call.actor_name + "*", "pActor", false, "new " + call.actor_name)
    #fun.add_unit(var)

    @actor_writer.add_unit_to_sectin("private_methods", fun)
  end

  def write_message_call_case(fun, call)
    fun.custom_code << "        case e_" + call.actor_name + "_" + call.name + " :"
    fun.custom_code << "        {"
    fun.custom_code << "            call_id_t call_id = {message.ac_id, message.req_num};"
    if not call.input_type.empty?
      fun.custom_code << "            " + call.input_type + "* pData = reinterpret_cast<" + call.input_type + "*>(message.data);"
      fun.custom_code << "            if(pData && (ac_status_exiting != m_status))"
      fun.custom_code << "            {"
      fun.custom_code << "                m_ptrHelper->start_measurement();"
      fun.custom_code << "                ret = " + call.name + "(call_id, *pData);"
      fun.custom_code << "                m_ptrHelper->stop_measurement(message.type);"
      fun.custom_code << "            }"
      fun.custom_code << "            ac_memory_alloctor<" + call.input_type + ">::instance().deallocate(pData);"
    else
      fun.custom_code << "            if(ac_status_exiting != m_status)"
      fun.custom_code << "            {"
      fun.custom_code << "                m_ptrHelper->start_measurement();"
      fun.custom_code << "                ret = " + call.name + "(call_id);"
      fun.custom_code << "                m_ptrHelper->stop_measurement(message.type);"
      fun.custom_code << "            }"
    end
    fun.custom_code << "            break;"
    fun.custom_code << "        }"
  end

  def write_message_res_case(fun, call)
    fun.custom_code << "        case e_" + call.actor_name + "_" + call.name + "_response :"
    fun.custom_code << "        {"
    if not call.output_type.empty?
      fun.custom_code << "            " + call.output_type + "* pData = reinterpret_cast<" + call.output_type + "*>(message.data);"
      fun.custom_code << "            if(pData && (ac_status_exiting != m_status))"
      fun.custom_code << "            {"
      fun.custom_code << "                m_ptrHelper->start_measurement();"
      fun.custom_code << "                ret = " + call.actor_name + "_" + call.name + "_response(message.req_num, *pData);"
      fun.custom_code << "                m_ptrHelper->stop_measurement(message.type);"
      fun.custom_code << "            }"
      fun.custom_code << "            ac_memory_alloctor<" + call.output_type + ">::instance().deallocate(pData);"
    else
      fun.custom_code << "            if(ac_status_exiting != m_status)"
      fun.custom_code << "            {"
      fun.custom_code << "                m_ptrHelper->start_measurement();"
      fun.custom_code << "                ret = " + call.actor_name + "_" + call.name + "_response(message.req_num);"
      fun.custom_code << "                m_ptrHelper->stop_measurement(message.type);"
      fun.custom_code << "            }"
    end
    fun.custom_code << "            break;"
    fun.custom_code << "        }"
  end

  def dumpit(w, filename)
    w.header(filename)
    w.println(%Q{#include "ac_global.h"})
    w.println(%Q{#include "ac_actor_type.h"})
    w.println(%Q{#include "ac_message_type.h"})
    w.println(%Q{#include "ac_actor.h"})
    w.println("#include \"" + filename + "_helper.h\"")
    w.line
    w.println("//custom include files")
    @include.each {
      |i|
      w.println(i)
    }
    w.line
    @actor_writer.dumpit(w)
    w.line
    w.tail(filename)
  end
end

#=============================================================
#                   Actor Helper Generator
#=============================================================
class ActorHelperWriter
  attr_reader :core, :data, :helper_writer, :call_outs, :syn_ac

  def initialize(core, data)
    @core = core
    @data = data
    @helper_writer = ClassGen.new("class", "public", data.name + "_helper")
    @call_outs = []
    @syn_ac = []
    collect

    @helper_writer.add_super("ac_helper")
    @helper_writer.add_sectin("constructor_methods", "public")
    @helper_writer.add_sectin("public_methods", "public")
    @helper_writer.add_sectin("private_methods", "private")
    @helper_writer.add_sectin("public_members", "public")
    @helper_writer.add_sectin("private_members", "private")

    write_constructor(data)
    write_set_id(data)
    write_members(data)

    @data.inits.each {
      |i|
      write_init_call(i)
    }
    write_exception_rep
    @data.calls.values.each {
      |a|
      write_call_ins(a)
    }
    @call_outs.each {
      |a|
      write_call_outs(a)
    }
  end

  def collect
    @data.calls.values.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }

    @data.inits.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }
  end

  def add_call(out)
    @call_outs.each {
      |o|
      if out.target_type == o.actor_name && o.name == out.call_name
        return
      end
    }
    if @core.actors[out.target_type] == nil
      return
    end

    c = @core.actors[out.target_type].calls[out.call_name]
    if c != nil
      @call_outs << c
    end
  end

  def write_constructor(actor)
    fun = MethodGen.new("", actor.name + "_helper")
    fun.add_parameter("ac_id_t ac_id")
    fun.add_initializer("ac_helper(ac_id)")
    fun.hasCode = true
    @helper_writer.add_unit_to_sectin("constructor_methods", fun)
    fun = MethodGen.new("virtual", "~" + actor.name + "_helper")
    fun.hasCode = true
    @helper_writer.add_unit_to_sectin("constructor_methods", fun)
  end

  def write_members(data)
    if $actor_id_map[data.name] != nil
      me = VarGen.new("", $actor_id_map[data.name], "m_id", false, "")
      @helper_writer.add_unit_to_sectin("private_members", me)
    end
  end

  def write_set_id(actor)
    if $actor_id_map[actor.name] != nil
      fun = MethodGen.new("void", "set_id")
      fun.add_parameter("const " + $actor_id_map[actor.name] + "&" + " id");
      fun.custom_code << "    m_id = id;\n"
      @helper_writer.add_unit_to_sectin("constructor_methods", fun)

      fun = MethodGen.new($actor_id_map[actor.name], "get_id")
      fun.custom_code << "    return m_id;\n"
      @helper_writer.add_unit_to_sectin("constructor_methods", fun)
    end
  end

  def write_call_ins(call)
    if call.async
      res = MethodGen.new("bool", call.actor_name + "_" + call.name + "_respond")
      res.add_parameter("call_id_t call_id");
      output_type = ""
      if not call.output_type.empty?
        output_type = call.output_type
        res.add_parameter(call.output_type + "&" +" output");
      end
      @helper_writer.add_unit_to_sectin("public_methods", res)
    end
  end

  def get_id_type(actor)
    if $actor_id_map[actor] == nil
      return ""
    else
      return $actor_id_map[actor]
    end
  end

  def write_call_outs(call)
    if not call.async
      # sync call with actor_id
      req = MethodGen.new("bool", call.actor_name + "_" + call.name)
      if not call.actor_is_singleton
        req.add_parameter(get_id_type(call.actor_name) + " id");
      end
      if not call.input_type.empty?
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      if not call.output_type.empty?
        req.add_parameter(call.output_type + "&" + " output");
      end
      @helper_writer.add_unit_to_sectin("public_methods", req)

      # sync call with duke_id
      # req = MethodGen.new("bool", call.actor_name + "_" + call.name)
      # req.add_parameter("duke_id_t id");
      # if not call.input_type.empty?
      #   req.add_parameter("const " + call.input_type + "&" + " input");
      #end
      #if not call.output_type.empty?
      #  req.add_parameter(call.output_type + "&" + " output");
      #end
      # @helper_writer.add_unit_to_sectin("public_methods", req)

      # sync call without actor_id
      # req = MethodGen.new("bool", call.actor_name + "_" + call.name)
      #if not call.input_type.empty?
      #  req.add_parameter("const " + call.input_type + "&" + " input");
      #end
      #if not call.output_type.empty?
      #  req.add_parameter(call.output_type + "&" + " output");
      #end
      # @helper_writer.add_unit_to_sectin("public_methods", req)

      #if not @syn_ac.include? call.actor_name
      #  @syn_ac << call.actor_name
      #end
    else
      req = MethodGen.new("bool", call.actor_name + "_" + call.name)
      if not call.actor_is_singleton
        req.add_parameter(get_id_type(call.actor_name) + " id");
      end
      req.add_parameter("req_num_t req_num");
      input_type = ""
      if not call.input_type.empty?
        input_type = call.input_type
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      @helper_writer.add_unit_to_sectin("public_methods", req)
    end
  end

  def write_init_call(init)
    if init.async
      rep = MethodGen.new("bool", "initialization_respond")
      @helper_writer.add_unit_to_sectin("public_methods", rep)
      rep = MethodGen.new("bool", "initialization_respond_fail")
      @helper_writer.add_unit_to_sectin("public_methods", rep)
    end
  end

  def write_exception_rep()
    rep = MethodGen.new("bool", "exception_respond")
    rep.add_parameter("call_id_t call_id");
    rep.add_parameter("std::string& output");
    @helper_writer.add_unit_to_sectin("public_methods", rep)
  end

  def dumpit(w, filename)
    w.header(filename)
    w.println(%Q{#include "ac_actor_type.h"})
    w.println(%Q{#include "ac_manager.h"})
    w.println(%Q{#include "ac_helper.h"})
    w.line
    @helper_writer.dumpit(w)
    w.line
    w.println("typedef std::tr1::shared_ptr<" + filename + "> " + filename + "_ptr;")
    w.line
    w.tail(filename)
  end
end

#=============================================================
#                   Actor Helper Source Generator
#=============================================================
class ActorHelperSrcWriter
  attr_reader :core, :data, :call_outs, :syn_ac, :methods

  def initialize(core, data)
    @core = core
    @data = data
    @call_outs = []
    @syn_ac = []
    @methods = []
    @class_name =  data.name + "_helper"
    collect

    @data.inits.each {
      |i|
      write_init_call(i)
    }
    write_exception_rep
    @data.calls.values.each {
      |a|
      write_call_ins(a)
    }
    @call_outs.each {
      |a|
      write_call_outs(a)
    }
  end

  def collect
    @data.calls.values.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }

    @data.inits.each {
      |a|
      a.call_outs.each {
        |ao|
        add_call(ao)
      }
    }
  end

  def add_call(out)
    @call_outs.each {
      |o|
      if out.target_type == o.actor_name && o.name == out.call_name
        return
      end
    }
    if @core.actors[out.target_type] == nil
      return
    end

    c = @core.actors[out.target_type].calls[out.call_name]
    if c != nil
      @call_outs << c
    end
  end

  def get_id_type(actor)
    if $actor_id_map[actor] == nil
      return ""
    else
      return $actor_id_map[actor]
    end
  end

  def write_call_ins(call)
    if call.async
      res = MethodGen.new("bool", @class_name + "::" + call.actor_name + "_" + call.name + "_respond")
      res.add_parameter("call_id_t call_id");
      output_type = ""
      if not call.output_type.empty?
        output_type = call.output_type
        res.add_parameter(call.output_type + "&" +" output");
      end
      add_respond_impl(res, output_type, call)
      @methods << res
    end
  end

  def write_init_call(init)
    if init.async
      rep = MethodGen.new("bool", @class_name + "::initialization_respond")
      add_init_respond_impl(rep)
      @methods << rep

      rep = MethodGen.new("bool", @class_name + "::initialization_respond_fail")
      add_init_respond_fail_impl(rep)
      @methods << rep
    end
  end

  def write_exception_rep()
    rep = MethodGen.new("bool", @class_name + "::exception_respond")
    rep.add_parameter("call_id_t call_id");
    rep.add_parameter("std::string& output");
    add_exception_respond_impl(rep)
    @methods << rep
  end

  def write_call_outs(call)
    if not call.async
      # sync call with actor_id if is not singleton
      req = MethodGen.new("bool", @class_name + "::" + call.actor_name + "_" + call.name)
      if not call.actor_is_singleton
        req.add_parameter(get_id_type(call.actor_name) + " id");
      end
      if not call.input_type.empty?
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      if not call.output_type.empty?
        req.add_parameter(call.output_type + "&" + " output");
      end
      add_syn_call_impl(req, call, true)
      @methods << req

      # sync call with duke_id
      #req = MethodGen.new("bool", @class_name + "::" + call.actor_name + "_" + call.name)
      #req.add_parameter("duke_id_t id");
      #if not call.input_type.empty?
      #  req.add_parameter("const " + call.input_type + "&" + " input");
      #end
      #if not call.output_type.empty?
      #  req.add_parameter(call.output_type + "&" + " output");
      #end
      #add_syn_call_impl(req, call, true)
      # @methods << req

      # sync call without actor_id
      # req = MethodGen.new("bool",  @class_name + "::" + call.actor_name + "_" + call.name)
      #if not call.input_type.empty?
      #  req.add_parameter("const " + call.input_type + "&" + " input");
      #end
      #if not call.output_type.empty?
      #  req.add_parameter(call.output_type + "&" + " output");
      #end
      #add_syn_call_impl(req, call, false)
      # @methods << req

      if not @syn_ac.include? call.actor_name
        @syn_ac << call.actor_name
      end
    else
      # async call with actor_id if is not singleton
      req = MethodGen.new("bool",  @class_name + "::" + call.actor_name + "_" + call.name)
      if not call.actor_is_singleton
        req.add_parameter(get_id_type(call.actor_name) + " id");
      end
      req.add_parameter("req_num_t req_num");
      input_type = ""
      if not call.input_type.empty?
        input_type = call.input_type
        req.add_parameter("const " + call.input_type + "&" + " input");
      end
      add_request_impl(req, input_type, call)
      req.hasCode = true
      @methods << req
    end
  end

  def add_init_respond_impl(rep)
    rep.custom_code << "    ac_actor * pActor = ac_manager::instance().acid_to_actor(m_ac_id);"
    rep.custom_code << "    if(pActor)"
    rep.custom_code << "    {"
    rep.custom_code << "        pActor->set_initialized_status();"
    rep.custom_code << "        return true;"
    rep.custom_code << "    }"
    rep.custom_code << "    return false;"
  end

  def add_init_respond_fail_impl(rep)
    rep.custom_code << "    ac_actor * pActor = ac_manager::instance().acid_to_actor(m_ac_id);"
    rep.custom_code << "    if(pActor)"
    rep.custom_code << "    {"
    rep.custom_code << "        pActor->set_initialized_fail_status();"
    rep.custom_code << "        return true;"
    rep.custom_code << "    }"
    rep.custom_code << "    return false;"
  end

  def add_exception_respond_impl(rep)
    rep.custom_code << "    pause_measurement();"
    rep.custom_code << "    std::string* pData = ac_memory_alloctor<std::string>::instance().allocate();"
    rep.custom_code << "    "
    rep.custom_code << "    *pData = output;"
    rep.custom_code << "    bool ret = ac_manager::instance().send_asyn_message(call_id.ac_id, m_ac_id, call_id.req_num, e_ac_actor_exception, pData);;"
    rep.custom_code << "    renew_measurement();"
    rep.custom_code << "    return ret;"
  end

  def add_syn_call_impl(req, call, hasId)
    req.custom_code << "    pause_measurement();"
    req.custom_code << "    bool ret = false;"
    if call.actor_is_singleton
      req.custom_code << "    ac_id_t dest_id = g_" + call.actor_name + "_acid;\n\n"
    else
      req.custom_code << "    ac_id_t dest_id;"
      req.custom_code << "    if(!ac_manager::instance().get_ac_id(id, dest_id))"
      req.custom_code << "    {"
      req.custom_code << "        renew_measurement();"
      req.custom_code << "        return false;\n\n"
      req.custom_code << "    }"
    end

    if hasId
      req.custom_code << "    " + call.actor_name + "* pActor = dynamic_cast<" + call.actor_name + "*>(ac_manager::instance().acid_to_actor(dest_id));"
    else
      req.custom_code << "    " + call.actor_name + "* pActor = dynamic_cast<" + call.actor_name + "*>(ac_manager::instance().request_actor(e_" + call.actor_name + "));"
    end

    req.custom_code << "    if(pActor)"
    req.custom_code << "    {"
    req.custom_code << "        renew_measurement();"
    req.custom_code << "        boost::recursive_mutex::scoped_lock lock(pActor->get_sync_mutex());"
    custom_code = "        ret = pActor->" + call.name
    if call.input_type.empty?
      if call.output_type.empty?
        custom_code = custom_code + "();\n"
      else
        custom_code = custom_code + "(output);\n"
      end
    else
      if call.output_type.empty?
        custom_code = custom_code + "(input);\n"
      else
        custom_code = custom_code + "(input, output);\n"
      end
    end
    req.custom_code << custom_code
    req.custom_code << "        pause_measurement();"
    req.custom_code << "    }"
    req.custom_code << "    renew_measurement();"
    req.custom_code << "    return ret;"
  end

  def add_request_impl(req, input_type, call)
    req.custom_code << "    pause_measurement();"
    req.custom_code << "    bool ret = false;"
    if call.remoteable
 
      req.custom_code << "    host_id_t host_id;"
      if call.actor_is_singleton
        req.custom_code << "    if(!input.get_host_id(host_id))"
      else
        req.custom_code << "    if(!id.get_host_id(host_id))"
      end
      req.custom_code << "    {"
      req.custom_code << "        renew_measurement();"
      req.custom_code << "        return false;"
      req.custom_code << "    }\n\n"
      req.custom_code << "    if(host_id != host_manager::instance().get_local_host_id())"
      req.custom_code << "    {"
      if call.actor_is_singleton
        req.custom_code << "        nb_singleton_id_t id(host_id, static_cast<uint32_t>(e_" + call.actor_name +  "));"
      end
      if @data.is_singleton
        req.custom_code << "        nb_singleton_id_t m_id(host_id, static_cast<uint32_t>(e_" + @data.name +  "));"
      end
      req.custom_code << "        ret =  ac_manager::instance().send_remote_message(id, m_id, req_num, "  + "e_" + call.actor_name + "_" + call.name + ", input, false);\n"
      req.custom_code << "        renew_measurement();"
      req.custom_code << "        return ret;"
      req.custom_code << "    }\n\n"
    end
    
    if call.actor_is_singleton
      req.custom_code << "    ac_id_t dest_id = g_" + call.actor_name + "_acid;\n\n"
    else
      req.custom_code << "    ac_id_t dest_id;\n\n"
      req.custom_code << "    if(!ac_manager::instance().request_actor(id, dest_id))"
      req.custom_code << "    {"
      req.custom_code << "        renew_measurement();"
      req.custom_code << "        return false;"
      req.custom_code << "    }"
    end

    input_data = "NULL"
    if not input_type.empty?
      req.custom_code << "    " + input_type + "* pData = ac_memory_alloctor<" + input_type + ">::instance().allocate();"
      req.custom_code << "    *pData = input;"
      input_data = "pData"
    end

    req.custom_code << "    ret = ac_manager::instance().send_asyn_message(dest_id, m_ac_id, req_num, "  + "e_" + call.actor_name + "_" + call.name + ", " + input_data + ");\n"
    req.custom_code << "    renew_measurement();"
    req.custom_code << "    return ret;"
  end

  def add_respond_impl(res, output_type, call)
    res.custom_code << "    pause_measurement();"
    res.custom_code << "    bool ret = false;"
    if not output_type.empty?
      res.custom_code << "    " + output_type + "* pData = ac_memory_alloctor<" + output_type + ">::instance().allocate();"
      res.custom_code << "    *pData = output;"
      res.custom_code << "    ret = ac_manager::instance().send_asyn_message(call_id.ac_id, m_ac_id, call_id.req_num, "  + "e_" + call.actor_name + "_" + call.name + "_response, pData, true);\n"
    else
      res.custom_code << "    ret = ac_manager::instance().send_asyn_message(call_id.ac_id, m_ac_id, call_id.req_num, "  + "e_" + call.actor_name + "_" + call.name + "_response, NULL, true);\n"
    end
    res.custom_code << "    renew_measurement();"
    res.custom_code << "    return ret;"
  end

  def dumpit(w, filename)
    w.header("")
    w.println(%Q{#include "ac_actor_type.h"})
    w.println(%Q{#include "ac_message_type.h"})
    w.println(%Q{#include "ac_manager.h"})
    w.println(%Q{#include "ac_actor.h"})
    w.println(%Q{#include "nb_host_manager.h"})
    w.println("#include \"" + filename + ".h\"")
    w.line
    @syn_ac.each {
      |a|
      w.println("#include \"" + a + ".h\"")
    }
    w.line
    @methods.each {
      |f|
      f.dumpit(w)
      w.line
    }
    w.line
    w.tail("")
  end
end

#=============================================================
#                    nb_version Generator
#=============================================================
class NBVersionWriter

  def initialize(base_dir)
  # str = `svn info #{base_dir}../..`
  # @ver_str = "0.0.0." + str[str.index("Revision: ") + 10, 
  #                           str.index("Node Kind: ") - str.index("Revision: ") - 11]
  # @time_str = str[str.index("Last Changed Date: ") + 20, 
  #                 str.size  - str.index("Last Changed Date: ") - 22]
    @ver_str = "0.0.0.0"
    @time_str = "0.0.0.0"#Time.new.to_s
    @build_str = "0.0.0.0"#Time.new.to_s
  end

  def dumpit(w, filename)
    w.header(filename)
    w.line
    w.println("#include <string>")
    w.line
    w.println("const std::string nb_version_string(\"" + @ver_str + "\");")
    w.line
    w.println("const std::string nb_version_time_string(\"" + @time_str + "\");")
    w.line
    w.println("const std::string nb_build_time_string(\"" + @build_str +"\");")
    w.line
    w.tail(filename)
  end
end


#=============================================================
#                   Main Generator
#=============================================================

if __FILE__ == $0
  o = Core.new("./res")
  o.print_error("core")

  base_dir = "../../../src/core/"

  if not File.exist?(base_dir + "actor/include")
    FileUtils.mkdir(base_dir + "actor/inlcude")
  end

  fn = "ac_message_type"
  temp_fn = "temp_" + fn
  w = CodeWriter.into_file(base_dir + "include/" + temp_fn + ".h" )
  tw = MessageTypeWriter.new(o)
  tw.dumpit(w, fn)
  cmp = CodeCompare.new(base_dir + "include/" + fn + ".h", base_dir + "include/" + temp_fn + ".h")
  cmp.replace_if_diff()

  fn = "remote_message_helper"
  temp_fn = "temp_" + fn
  w = CodeWriter.into_file(base_dir + "include/" + temp_fn + ".h" )
  tw = RemoteMessageHelperWriter.new(o)
  tw.dumpit(w, fn)
  cmp = CodeCompare.new(base_dir + "include/" + fn + ".h", base_dir + "include/" + temp_fn + ".h")
  cmp.replace_if_diff()

  fn = "ac_actor_type"
  temp_fn = "temp_" + fn
  w = CodeWriter.into_file(base_dir + "include/" + temp_fn + ".h" )
  tw = ActorTypeWriter.new(o)
  tw.dumpit(w, fn)
  cmp = CodeCompare.new(base_dir + "include/" + fn + ".h", base_dir + "include/" + temp_fn + ".h")
  cmp.replace_if_diff()

  fn = "ac_factory"
  temp_fn = "temp_" + fn
  w = CodeWriter.into_file(base_dir + "framework/src/" + temp_fn + ".cpp" )
  tw = ActorFactoryWriter.new(o)
  tw.dumpit(w, fn)
  cmp = CodeCompare.new(base_dir + "framework/src/" + fn + ".cpp", 
                        base_dir + "framework/src/" + temp_fn + ".cpp")
  cmp.replace_if_diff()

  o.actors.each do |k, a|
    temp_k = "temp_" + k
    w = CodeWriter.into_file(base_dir + "actor/include/" + temp_k + ".h")
    aw = ActorWriter.new(o, a)
    aw.dumpit(w, k)
    cmp = CodeCompare.new(base_dir + "actor/include/" + k + ".h", 
                          base_dir + "actor/include/" + temp_k + ".h")
    cmp.replace_if_diff()


    hw = CodeWriter.into_file(base_dir + "actor/include/" + temp_k + "_helper.h")
    ahw = ActorHelperWriter.new(o, a)
    ahw.dumpit(hw, k + "_helper")
    cmp = CodeCompare.new(base_dir + "actor/include/" + k + "_helper.h", 
                          base_dir + "actor/include/" + temp_k + "_helper.h")
    cmp.replace_if_diff()

    hs = CodeWriter.into_file(base_dir + "actor/src/" + temp_k + "_helper.cpp")
    ahs = ActorHelperSrcWriter.new(o, a)
    ahs.dumpit(hs, k + "_helper")
    cmp = CodeCompare.new(base_dir + "actor/src/" + k + "_helper.cpp", 
                          base_dir + "actor/src/" + temp_k + "_helper.cpp")
    cmp.replace_if_diff()
  end

  fn = "nb_version"
  temp_fn = "temp_" + fn
  w = CodeWriter.into_file(base_dir + "include/" + temp_fn + ".h" )
  vw = NBVersionWriter.new(base_dir)
  vw.dumpit(w, fn)
  cmp = CodeCompare.new(base_dir + "include/" + fn + ".h", 
                        base_dir + "include/" + temp_fn + ".h")
  cmp.replace_if_diff()

end
