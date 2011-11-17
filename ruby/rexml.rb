require 'rexml/document'
include REXML

input = File.new("test.xml")
doc = Document.new(input)

root = doc.root
puts root.attributes["name"]

#doc.elements.each("library/book") { |e| puts e }
root.each_element { |e| puts e }
