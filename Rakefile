require 'rake'
require 'rake/clean'

BIN = 'ebin'
INCLUDE = 'include'
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"
CLEAN.include(["#{BIN}/*.beam", '*.dump'])
SRC = FileList['src/*.erl','src/*.hrl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")

directory BIN

namespace :erlang do
    desc "starting qerl"
    task :run => [:compile] do
        sh("erl -noshell -pa #{BIN} -s qerl_example_server start")
    end
end

desc "initial project setup"
task :init do
    FileUtils.mkdir_p BIN
end 

desc "default set to 'compile'"
task :default => [:compile]

desc "start the erlang application"
task :start => ['erlang:run']

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
      sh "erlc -pa #{BIN} -W #{ERLC_FLAGS} -o #{BIN} #{t.source}"
end

desc "compile the current sources"
task :compile => ['ebin'] + OBJ

