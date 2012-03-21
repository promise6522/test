#! /usr/bin/env ruby1.9.1

require 'ostruct'
require 'socket'
require 'net/ssh'
require 'net/scp'

##===========================================================#
##
##               Configurations Fields
##
##===========================================================#
# Source Database dirs :
$INCLUDE_DIRS = [ "dbcore", "dbmedia" ]

# Directories to sync between :
# sync data based on current dir (it must be in the list)
$SYNC_DIRS = [    
    # example format:
    #"192.168.1.18:/home/tom/IS/bin/",
    #"192.168.1.18:/home/tom/penguin/build/",
    #"192.168.1.18:/home/tom/bridge_entry/",
    #"192.168.1.19:/home/tom/penguin/build/",
]

# Leave the tarball in other sync dirs?
$KEEP_TAR = false

##===========================================================#
##               End of Configuration 
##===========================================================#

class Logger
    @@Text_red      = "\033[22;31m"
    @@Text_green    = "\033[22;32m"
    @@Text_dark     = "\033[01;30m"
    @@Text_endcolor = "\033[0m"

    def self.err(msg)
        puts @@Text_red + msg + @@Text_endcolor
        # exit on error
        exit(-1)
    end

    def self.info(msg)
        puts @@Text_green + msg + @@Text_endcolor
    end

    def self.prompt(msg)
        puts @@Text_dark + msg + @@Text_endcolor
    end
end 

def sys_cmd(cmd)
    Logger.prompt("Local : #{cmd}")
    if not system(cmd)
        # if failed, we report and exit
        Logger.err("System command '#{cmd}' failed.")
    end
end

def check_config(local_ip)
    # remove current dir from the sync list
    new_sync_list = $SYNC_DIRS.reject { |item| item.chomp("/") == "#{local_ip}:#{Dir.pwd}" }
    if $SYNC_DIRS == new_sync_list
        Logger.err("Local directory must be in the sync list!")
    end
    $SYNC_DIRS = new_sync_list

    # parse ip/path/user
    results = []
    $SYNC_DIRS.each { |value|
        parts = value.partition(":")
        if parts[1].empty? or parts[2].empty?
            Logger.err("Invalid target dir : must be in 'ip:path' format.")
        end
        ip = parts[0]
        path = parts[2]

        # from 192.168.1.1 to 192.168.1.254
        if ip[0, "192.168.1.".length] == "192.168.1." and ip["192.168.1.".length, ip.length-1].to_i.between?(1, 254)
        else
            Logger.err("Invalid IP '#{ip}' : only IP from 192.168.1.1 to 192.168.1.254 is allowed.")
        end

        # check paths and extract the username
        groups = path.match(/^\/home\/([^\/]+)\/.*$/)
        if groups 
            user = groups[1]
        else
            Logger.err("Invalid path #{path}.")
        end

        results << OpenStruct.new(:ip => ip, :path => path, :user => user)
    } 

    return results
end

def get_local_ip()
    addrinfo = Socket.ip_address_list.detect { |intf|
        intf.ipv4? and !intf.ipv4_loopback? and !intf.ipv4_multicast?
    }
    if addrinfo.nil?
        Logger.err("Get local ip address failed.")
    else
        return addrinfo.ip_address
    end
end


if __FILE__ == $0
    # get local ip address
    local_ip = get_local_ip()
    Logger.info("Local IP Address : #{local_ip}")

    # get a list of {ip, path, username}
    results = check_config(local_ip)

    # get tarball name from AGRV[0]
    if ARGV.size == 1
        zip_name = "#{ARGV[0]}.tgz"
    else
        Logger.err("usage : #{__FILE__} <tarball_name>")
    end

    # archive and compress
    zip_cmd = "tar cvf - "
    $INCLUDE_DIRS.each { | dir |
        if File.directory?(dir)
            zip_cmd += " #{dir}"
        else
            # dir not exists, exit
            Logger.err("can't find dir '#{dir}' in current dir")
        end
    }
    zip_cmd += " | gzip --fast -c > #{zip_name}"
    sys_cmd(zip_cmd)

    # for all the paths in the list, operate on the peer type (local or remote)
    results.each { |value|
        ip = value.ip
        path = value.path
        user = value.user

        if ip == local_ip 
            # check if dest path exsits
            if not File.directory?(path)
                Logger.err("Target dir '#{path}' not found on #{ip}")
            end

            # cp the tarball
            sys_cmd("cp #{zip_name} #{path}")

            # change to the dest dir temporarily
            Dir.chdir(path) do 
                # remove existing dirs
                $INCLUDE_DIRS.each { | dir |
                    if File.directory?(dir)
                        sys_cmd("rm -rf #{dir}")
                    end
                }

                # unzip the tarball
                sys_cmd("tar xvf #{zip_name}")

                # remove the tarball
                sys_cmd("rm #{zip_name}") unless $KEEP_TAR
            end

        else

            # unzip the tarball using SSH
            Net::SSH.start(ip, user, :password => user) do |ssh|
                # validate path
                ssh.exec!("cd #{path}") { |ch, stream, data|
                    if stream == :stdLog.err
                        Logger.err("SSH Error : #{data}")
                    end
                }

                # upload the tarball using SCP
                Net::SCP.start(ip, user, :password => user) do |scp|
                    Logger.info("#{user}@#{ip} : Uploading #{zip_name} to #{path} ......")
                    scp.upload!(zip_name, path, {:verbose => true})
                    Logger.info("#{user}@#{ip} : Upload complete.")
                end
                
                # remove existing db dir
                $INCLUDE_DIRS.each { |dir|
                    ssh.exec!("rm -rf #{path}/#{dir}")
                }

                # unzip the tarball
                Logger.info("#{user}@#{ip} : Extracting #{zip_name} to #{path} ......")
                ssh.exec!("cd #{path} && tar xvf #{zip_name}") { |ch, stream, data|
                    if stream == :stderr
                        Logger.err("SSH Error : #{data}")
                    end
                }
                Logger.info("#{user}@#{ip} : Extraction complete.")

                # remove the tarball
                ssh.exec!("rm #{path}/#{zip_name}") unless $KEEP_TAR
            end
        end
    }

end
