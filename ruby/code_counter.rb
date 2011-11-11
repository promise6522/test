#! /usr/bin/env ruby

require 'find'

$SrcPattern = /.*\.h|.*\.cpp/

# check arg num
unless ARGV.size == 1
  puts "usage : code_counter.rb filename"
  exit -1
end

# check if a valid path
is_file = File.stat(ARGV[0]).file?
is_dir = File.stat(ARGV[0]).directory?
unless is_file || is_dir
  puts "error : must specify a file/directory path"
  exit -1
end

total = 0

# for a file, count the line
if is_file
  total = File.open(ARGV[0]).readlines.size
end

# for a directory, traverse
if is_dir
  Find.find(ARGV[0]) do | path |
      # see if matches file pattern
      unless path.match($SrcPattern)
        puts "Ignore non-source fie : #{path}"
        next
      end

      # check only regular file
      if File.stat(path).file?
        line = File.open(path).readlines.size
        puts path.ljust(73, " ") + line.to_s.rjust(6, " ")
        total += line
      end
  end
end

puts "Total line count : #{total}"
    
