#!/usr/bin/env ruby
DIR_BUILD = "../build"


#CMD_CMAKE = "cmake .."
CMD_CMAKE = "cmake -D CMAKE_BUILD_TYPE=Debug -D CMAKE_CXX_COMPILER=ccache -D CMAKE_CXX_COMPILER_ARG1=g++ -D CMAKE_C_COMPILER=ccache -D CMAKE_C_COMPILER_ARG1=gcc .."
CMD_MAKE  = "make"
CMD_CLEAN = "rm -rf *"

class SnowTest
  # run cmake to generate Makefile
  def run_cmake()
    result = false
    Dir.chdir(DIR_BUILD) do
      result = system(CMD_CMAKE)
      puts "storage cmake failed, and return #{result}." unless result
    end
    return result
  end

  # run make program to generate exetable file
  def run_make()
    result = false
    Dir.chdir(DIR_BUILD) do
      result = system(CMD_MAKE)
      puts "storage make failed, and return #{result}." unless result
    end
    return result
  end

  # clean build directory
  def run_clean()
    result = false
    Dir.chdir(DIR_BUILD) do
      result = system(CMD_CLEAN)
      puts "clean build directory failed, and return #{result}." unless result
    end
    return result
  end

  # run all tests
  def run()
    result = run_cmake
    return result unless result

  # result = run_make
  # return result unless result

  # puts "cmake, make succeed."

    return result
  end
end

if __FILE__ == $0
  snow = SnowTest.new
  snow.run
end

# vim:set tabstop=2 shiftwidth=2 expandtab:
