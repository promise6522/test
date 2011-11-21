require 'rexml/document'
include REXML

# bridge object defination
$bridge_defs = []

# Struct BridgeDef
BridgeDef = Struct.new(:name, :index, :entries)

# Struct BridgeEntry
BridgeEntry = Struct.new(:name, :type, :is_array, :default_value)

input = File.new("bridge.xml")
doc = Document.new(input)

root = doc.root
index = 0

root.each_element("type") do | e |
  # index from 1
  index += 1
  name = e.attributes["name"]
  doc = e.text
  entries = []
  e.each_element("entry") do | en |
    entry_name = en.attributes["name"]
    type = en.attributes["type"]
    is_array = en.attributes["isArray"] ? true : false 
    default_value  = en.attributes["defaultValue"]
    entries << BridgeEntry.new(entry_name, type, is_array, default_value)
  end
  $bridge_defs << BridgeDef.new(name, index, entries)
end

puts "index = #{index}"

$bridge_funcs = []

$bridge_defs.each do | e |

  # one line for bridge name as comment
  $bridge_funcs << "// #{e.index}: #{e.name}\n"

  # one line for base instruction index
  base = "NB_BRIDGE_FUNC_#{e.name.upcase}_INSTRUCTION".ljust(80, " ") + " = 0x%02X00,\n" % [e.index]
  $bridge_funcs << base

  #
  e.entries.each_with_index do | entry, index |
    # we assume max 255 bridge objects, each with max 255 fields
    enum_id = "0x%02X%02X" % [ e.index, index*2+1 ]
    getter = "NB_BRIDGE_FUNC_#{e.name.upcase}_GET_#{entry.name.upcase}".ljust(80, " ") + " = #{enum_id},\n"
    enum_id = "0x%02X%02X" % [ e.index, index*2+2 ]
    setter = "NB_BRIDGE_FUNC_#{e.name.upcase}_SET_#{entry.name.upcase}".ljust(80, " ") + " = #{enum_id},\n"
    $bridge_funcs << getter << setter
  end
  $bridge_funcs << "\n"

end

# write to nb_bridge_instruction.h
gen_name = "nb_bridge_instruction.h"
File.open(gen_name, "w") do | file |
  # writing head
  guard = "_#{gen_name.upcase.gsub(/\./, "_")}_"
  file.write("#ifndef #{guard}\n")
  file.write("#define #{guard}\n")
  file.write("\n")

  file.write("enum nb_bridge_instruction_t {\n")
  file.write("\n")

  # indent 4 spaces
  indent = " " * 4
  $bridge_funcs.each { | e | file.write(indent + e) }

  file.write("}; /* enum nb_builtin_instruction_t */\n")

  # writing end
  file.write("\n")
  file.write("#endif /* #{guard} */")
end

if __FILE__ == $0
  puts %x[cat nb_bridge_instruction.h]
end
