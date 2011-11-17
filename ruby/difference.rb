unless ARGV.size == 2
  puts "usage: difference.rb origin_file new_file"
  exit -1
end

old_list = File.open(ARGV[0]).readlines
new_list = File.open(ARGV[1]).readlines

puts "The following lines have been added:"
puts new_list - old_list

puts ""
puts "The following lines have been deleted:"
puts old_list - new_list
