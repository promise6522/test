#!/usr/bin/env ruby

require 'getoptlong'


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


CMD_BACKUP_UBUNTU_51  = "db5.1_hotbackup"   # Ubuntu
CMD_BACKUP_FREEBSD_51 = "db_hotbackup-5.1"  # FreeBSD
CMD_BACKUP = [
  CMD_BACKUP_UBUNTU_51,
  CMD_BACKUP_FREEBSD_51,
];

PATH_ROOT = "./"
PATH_DB_BRIDGE = "dbcore/dbbridge"
PATH_DB_CONTAINER = "dbcore/dbcontainer"
PATH_DB_ID = "dbcore/dbid"
PATH_DB_OBJECT = "dbcore/dbobject"
PATH_DB_STORAGE = "dbcore/dbstorage"
PATH_DB_FORMOBJECT = "dbmedia/dbformobject"
PATH_DB_INFO = "dbmedia/dbinfo"
PATH_DB_TEMPOBJECT = "dbmedia/dbtempobject"
PATH_DB_VERIFY = "dbmedia/dbverify"

PATH_DB = [
  PATH_DB_BRIDGE,
  PATH_DB_CONTAINER,
  PATH_DB_ID,
  PATH_DB_OBJECT,
  PATH_DB_STORAGE,
  PATH_DB_FORMOBJECT,
  PATH_DB_INFO,
  PATH_DB_TEMPOBJECT,
  PATH_DB_VERIFY,
];

class NbBackup
  def initialize(src_root, dst_root, verbose)
    @src_root = src_root
    @dst_root = dst_root
    @verbose = verbose
  end

  def print_startinfo(dirdata)
  # print_notice("Start to back up database: " + dirhome)
  # puts Text_red + text + Text_endcolor
    print Text_white + ">>>" + Text_endcolor + " Start to back up database: "
    print_notice(dirdata)
  end

  def print_result(result)
    if (result)
      print_warning("Succeeded")
    else
      print_error("Failed")
    end
    puts
  end

  def time_directory()
  # puts Time.now
    timepath = Time.now.strftime("%Y%m%d%H%M%S")
  # puts timepath
    return timepath
  end

  def find_backup_command
    CMD_BACKUP.each do |cmd|
      usrpath = "/usr/bin/"
      localpath = "/usr/local/bin/"
      return cmd if File::exist?(usrpath + cmd)
      return cmd if File::exist?(localpath + cmd)
    end
    print_error "can NOT find hotbackup command in this OS."
    return ""
  end

  def backup(cmd_backup, dirhome, dirdata, path_time)
    arghome = @src_root + dirhome;
    argdata = @src_root + dirdata;
    path_dest = path_time + "/" + dirdata;

    # make destination directory
    Dir.mkdir(path_dest) unless File.directory?(path_dest)

    # composite command
    fullcmd = cmd_backup
    fullcmd += " -v " if @verbose;
    fullcmd += " -h " + arghome + " -d " + argdata + " -b " + path_dest;
    puts fullcmd if @verbose;

    # run command
    result = false
    if check_path(arghome)
      result = system(fullcmd)
    else
      print_info("Skip #{arghome}, which does NOT exist!")
    end

    return result
  end

  def check_path(path)
    File.directory? path
  end

  def check_paths
    PATH_DB.each do |path|
      arghome = @src_root + dirhome;
      return false unless File.directory? arghome
    end
    return true
  end

  def run()
    # find hotbackup command
    cmd_backup = find_backup_command
    return false if cmd_backup.empty?

    # make destination directory
    dest_root = @dst_root
    Dir.mkdir(dest_root) unless File.directory?(dest_root)

    path_time = @dst_root + "/" + time_directory()
    Dir.mkdir(path_time) unless File.directory?(path_time)

    path_core = path_time + "/" + "dbcore"
    Dir.mkdir(path_core) unless File.directory?(path_core)

    path_media = path_time + "/" + "dbmedia"
    Dir.mkdir(path_media) unless File.directory?(path_media)

    PATH_DB.each do |path_home|
      print_startinfo(path_home)
      result = backup(cmd_backup, path_home, path_home, path_time)
      print_result(result)
    end
  end
end

def usage
  print_notice("NAME")
  print Text_white + "\tnb_backup.rb" + Text_endcolor + " -- back up database\n"
  puts

  print_notice("DESCRIPTION")
  print_info("\t-h, --help\t\tprint help information")
  print_info("\t-v, --verbose\t\tprint debug information")
  print_info("\t-s, --src\t\tdatabase source directory")
  print_info("\t-d, --dst\t\tdatabase backup directory")
  puts
end

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--verbose', '-v', GetoptLong::NO_ARGUMENT ],
  [ '--src', '-s', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--dst', '-d', GetoptLong::OPTIONAL_ARGUMENT ]
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
  when '--src'
    src_root = val
  when '--dst'
    dst_root = val
  end
end

=begin
if ARGV.size != 0 then
  dst_root = ARGV.first
end
=end

puts "backup directory is: " + dst_root
bdb = NbBackup.new(src_root, dst_root, verbose)
bdb.run

# vim:set tabstop=2 shiftwidth=2 expandtab:
