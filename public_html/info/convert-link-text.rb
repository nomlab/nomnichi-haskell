#!/usr/bin/env ruby

require 'pathname'

## usage: convet-check-orphan-links.rb

def ignoreable_link?(link)
  return true if link =~ /^(https?|mailto):/
  return true if link =~ /^#/
  # return true if link =~ /^.\/$/
  return false
end

def link_to_path(link, cwd = '.')
  orig_link = link

  if link =~ /^\//
    return 'http://www.swlab.cs.okayama-u.ac.jp' + link
  end

  link += "index.html" if link =~ /\/$/

  link = Pathname.new(link)
  link = cwd + link
end

def convert_link(link, cwd = '.')
  path = link_to_path(link, cwd).to_s

  unless path =~ /^http/
    path = path.gsub('/', '--')
    # path = 'wiki/' + path
    path = path.sub(/\.html$/, '')
  end

  return path
end

def check_orphan_link(link, cwd)
  path = link_to_path(link, cwd)

  STDERR.print "ABOUT: directory: #{cwd} link: #{link} expected-path: #{path}"

  if File.exist?(path)
    STDERR.print "...OK.\n"
    return false
  else
    STDERR.print "...NG.\n"
    return true
  end
end

def scan_file(file_name, cwd, &block)
  File.open(file_name, "r") do |file|
    while line = file.gets
      string = ""
      while line =~ /(href|src)="([^"]+)"/
        head, line, type, link = $`, $', $1, $2
        unless ignoreable_link?(link)
          link = yield(link)
        end
        string += "#{head}#{type}=\"#{link}\""
      end
      string += line
      print string
    end
  end
end

ARGV.each do |file_name|
  cwd = Pathname.new(File.dirname(file_name))

  unless file_name =~ /\.html$/
    print gets(nil)
  else
    scan_file(file_name, cwd) do |link|
      check_orphan_link(link, cwd)
      convert_link(link, cwd)
    end
  end
end
