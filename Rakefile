require 'rake'
require 'rake/clean'

BIN = 'ebin'
SRC_FOLDER = 'src'
ERLC_FLAGS = "+warn_unused_vars +warn_unused_import"
CLEAN.include(["#{BIN}/*.beam", '*.dump'])
SRC = FileList["#{SRC_FOLDER}/**/*.erl", "#{SRC_FOLDER}/**/*.hrl"]
BEAM = []

directory BIN

SRC.each do |fn|
    BEAM << dest = File.join(BIN, File.basename(fn).ext('beam'))
    file dest do
        sh "erlc -Ilib #{ERLC_FLAGS} -o #{BIN} #{fn} "
    end
end

namespace :erlang do
    desc "starting qerl"
    task :run => [:compile] do
        sh("erl -noshell -pa #{BIN} -s qerl_example_server start")
    end

    desc "run tests"
    task :test => BEAM do
        sh("erl -noshell -s test_my_mod test -s init stop")
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

desc "compile the current sources"
task :compile => [:init] + BEAM

