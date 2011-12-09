#!/usr/bin/env ruby

require 'getoptlong'
require 'net/ftp'
require 'uri'

Text_endcolor     = "\033[0m";
Text_black        = "\033[22;30m";
Text_red          = "\033[22;31m";
Text_green        = "\033[22;32m";
Text_brown        = "\033[22;33m";
Text_blue         = "\033[22;34m";
Text_magenta      = "\033[22;35m";
Text_cyan         = "\033[22;36m";
Text_gran         = "\033[22;37m";
Text_dark         = "\033[01;30m";
Text_lightred     = "\033[01;31m";
Text_lightgreen   = "\033[01;32m";
Text_yellow       = "\033[01;33m";
Text_lightblue    = "\033[01;34m";
Text_lightmagenta = "\033[01;35m";
Text_lightcyan    = "\033[01;36m";
Text_white        = "\033[01;37m";
Back_black        = "\033[22;40m";
Back_red          = "\033[22;41m";
Back_green        = "\033[22;42m";
Back_brown        = "\033[22;43m";
Back_blue         = "\033[22;44m";
Back_magenta      = "\033[22;45m";
Back_cyan         = "\033[22;46m";
Back_gran         = "\033[22;47m";
Back_dark         = "\033[01;40m";
Back_lightred     = "\033[01;41m";
Back_lightgreen   = "\033[01;42m";
Back_yellow       = "\033[01;43m";
Back_lightblue    = "\033[01;44m";
Back_lightmagenta = "\033[01;45m";
Back_lightcyan    = "\033[01;46m";
Back_white        = "\033[01;47m";

def print_error(text)
  puts Text_red + text + Text_endcolor
end

def print_warning(text)
  puts Text_yellow + text + Text_endcolor
end

def print_notice(text)
  puts Text_white + text + Text_endcolor
end

def print_info(text)
  puts text
end


# the current directory is script directory of penguin project
PATH_ROOT = ".."
PATH_BUILD = "../build"
PATH_SCRIPT = "../script/pre_make"
PATH_HOME = ENV['HOME']

CMD_BUILD_GENERATE = "ruby src/generator.rb"
CMD_BUILD_CMAKE    = "cmake -D CMAKE_BUILD_TYPE=Debug .."
CMD_BUILD_MAKE     = "make -j4"
CMD_BUILD_CLEAN    = "rm -rf *"


class NbRelease
  def initialize(version, debug)
    @numver = version.partition("-").last.rpartition(".").first
  # @pkgtag = version.upcase
    @pkgtag = version.downcase
    @pkgdir = version.downcase
    @pkghome = PATH_HOME + '/'
    @pkgpath = PATH_HOME + '/' + @pkgdir + '/'
    @pkgfile = PATH_HOME + '/' + @pkgdir + ".tar.gz"
    @debug = debug
    if @debug then
      print_notice "\nVersion is: #{version}\n"
      print_notice "\nNumber is: #{@numver}\n"
    end
  end

  def call_system_command(cmd)
    result = false
    if @debug then
      puts "Absolute path is: #{Dir.pwd}"
    # puts "\t#{cmd}"
      print "Command is: "
      print_warning " #{cmd}"
      puts
      result = true
    else
      result = system(cmd)
    end
  end

  # make svn tag for new release version
  def svn_make_tag()
    result = false
    Dir.chdir(PATH_ROOT) do
      cmd_svn_copy = "svn copy svn://192.168.1.5/penguin/trunk svn://192.168.1.5/penguin/tags/#{@pkgtag} -m \"make tag for release version #{@pkgtag}.\""
      result = call_system_command(cmd_svn_copy)
      puts "make tag failed, and return #{result}." unless result
    end
    return result
  end

  # make git tag for new release version
  def git_make_tag()
    result = false
    Dir.chdir(PATH_ROOT) do
      cmd_git_tag = "git tag -a #{@pkgtag} -m \"make tag for release version #{@pkgtag}.\""
      result = call_system_command(cmd_git_tag)
      puts "make tag failed, and return #{result}." unless result
    end
    return result
  end

  # update taged code
  def svn_update()
    result = false
    Dir.chdir(PATH_ROOT) do
      cmd_svn_update  = "svn up"
      result = system(cmd_svn_update)
      puts "updating source code failed, and return #{result}." unless result
    end
    return result
  end

  # push tag
  def git_push_tag()
    result = false
    Dir.chdir(PATH_ROOT) do
      cmd_git_push  = "git push origin #{@pkgtag}"
      result = call_system_command(cmd_git_push)
      puts "git push command failed, and return #{result}." unless result
    end
    return result
  end

  # generate source code from ruby script
  def generate
    puts "current path should be: " + PATH_SCRIPT + "/core"
    result = false
    Dir.chdir(PATH_SCRIPT+"/core") do
      result = call_system_command(CMD_BUILD_GENERATE)
      puts "generating source code failed, and return #{result}." unless result
    end
    Dir.chdir(PATH_SCRIPT+"/is") do
      result = system(CMD_BUILD_GENERATE)
      puts "generating source code failed, and return #{result}." unless result
    end
    return result
  end

  # run cmake to generate Makefile
  def run_cmake()
    result = false
    Dir.chdir(PATH_BUILD) do
      result = call_system_command(CMD_BUILD_CMAKE)
      puts "penguin cmake failed, and return #{result}." unless result
    end
    return result
  end

  # run make program to generate exetable file
  def run_make()
    result = false
    Dir.chdir(PATH_BUILD) do
      result = call_system_command(CMD_BUILD_MAKE)
      puts "penguin make failed, and return #{result}." unless result
    end
    return result
  end

  # make release package
  def make_pkg()
    result = false
    Dir.chdir(PATH_BUILD) do
      cmd_pkg_mkdir = "mkdir #{@pkgpath}"
    # result = system(cmd_pkg_mkdir)
      result = call_system_command(cmd_pkg_mkdir)
      puts "make #{@pkgdir} directory failed, and return #{result}." unless result

      nbd_file = "nbd-#{@numver}"
      cmd_pkg_bin   = "cp ./bin/nbd #{@pkgpath}#{nbd_file}"
      result = call_system_command(cmd_pkg_bin)
      puts "copy ndb file failed, and return #{result}." unless result

      cmd_pkg_script   = "cp ../script/nb_backup.rb #{@pkgpath}"
      result = call_system_command(cmd_pkg_script)
      puts "copy nb_backup.rb file failed, and return #{result}." unless result

      cmd_pkg_res   = "cp -Rv ../resource #{@pkgpath}"
      result = call_system_command(cmd_pkg_res)
      puts "copy resource files failed, and return #{result}." unless result

      cmd_pkg_make  = "cd #{@pkghome} && tar czvf #{@pkgfile} #{@pkgdir}"
      result = call_system_command(cmd_pkg_make)
      puts "make #{@pkgdir}.tar.gz file failed, and return #{result}." unless result
    end
    return result
  end

  # clean build directory
  def run_clean()
    result = false
    Dir.chdir(PATH_BUILD) do
      result = system(CMD_BUILD_CLEAN)
      puts "clean build directory failed, and return #{result}." unless result
    end
    return result
  end

  # clean pkg
  def clean_pkg
    cmd_pkg_clean = "rm -rf #{@pkgpath} && rm #{@pkgfile}"
    result = call_system_command(cmd_pkg_clean)
    puts "clean #{@pkgdir} directory failed, and return #{result}." unless result
  end

  # upload to ftp
  def upload()
    if @debug then
      puts "upload file #{@pkgfile} onto ftp://192.168.1.5/release/server"
    else
      uri = URI.parse('ftp://192.168.1.5')
      Net::FTP.open(uri.host) do |ftp|
        ftp.login 'ftp', 'ftp@nebutown.com'
        ftp.passive
        ftp.chdir '/release/server'
        ftp.putbinaryfile @pkgfile
      end
    end
    return true
  end

  # run all tests
  def run()
    result = git_make_tag
    return result unless result

    result = git_push_tag
    return result unless result

  # tag_path = "../../tags/#{@pkgtag}/build"
    tag_path = PATH_BUILD
    Dir.chdir(tag_path) do
      result = generate
      return result unless result

      result = run_cmake
      return result unless result

      result = run_make
      return result unless result

      result = make_pkg
      return result unless result
      puts "make package done."

    # result = run_clean
    # return result unless result
    end

    result = upload
    return result unless result
    puts "upload done."

    result = clean_pkg
    return result unless result
    puts "clean temporary files done."

    return result
  end
end

def usage
  print_notice("NAME")
  print Text_white + "\tnb_release.rb" + Text_endcolor + " -- release version\n"
  puts

  print_notice("SYNOPSIS")
  print Text_white + "\tnb_release.rb [-hv] <alpha-0.8.6>" + Text_endcolor
# puts "you should input a version like: ALPHA-0.1.2.10625 as argument."
  puts
  puts

  print_notice("DESCRIPTION")
  print_info("\t-h, --help\t\tprint help information")
  print_info("\t-v, --verbose\t\tprint debug information")
  puts
end

def get_git_version
  git_show = `git show HEAD`
  git_commit_num = git_show.partition("commit ").last.partition("\n").first
  return git_commit_num[0..6]
end

def get_svn_version
  # construct version
  svn_info = `svn info`
  svn_version = svn_info.partition("Revision: ").last.partition("\n").first
  return svn_version
end

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--verbose', '-v', GetoptLong::NO_ARGUMENT ]
# [ '--src', '-s', GetoptLong::REQUIRED_ARGUMENT ],
# [ '--dst', '-d', GetoptLong::OPTIONAL_ARGUMENT ]
)

# parsing command line arguments
src_root = "./"
dst_root = ENV['HOME'] + "/" + "dbbackup";
verbose = false
opts.each do |opt, val|
  case opt
  when '--help'
    usage
    exit
  when '--verbose'
    verbose = true
# when '--src'
#   src_root = val
# when '--dst'
#   dst_root = val
  end
end

if ARGV.size != 0 then
# argv = ARGV.first.grep(/^\w+-\d+\.\d+\.\d+$/)
  argv = ARGV.first.grep(/^(\w+)-(\d+)\.(\d+)\.(\d+)$/)
  build_ver = get_git_version
# build_ver = get_git_version($2, $3, $4)

  if argv.size() == 0 then
    usage
  else
    version = ARGV.first + "." + build_ver
    snow = NbRelease.new(version, verbose)
    result = snow.run
  end
else
  usage
end

# vim:set tabstop=2 shiftwidth=2 expandtab:
