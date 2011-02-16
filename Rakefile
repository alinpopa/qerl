require 'rake'
require 'rake/clean'

BIN = 'ebin'
TEST_BIN = 'tests_ebin'
INCLUDE = 'include'
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"
CLEAN.include(["#{BIN}/*.beam", "#{TEST_BIN}/*.beam", '*.dump'])
SRC = FileList['src/*.erl','src/*.hrl']
TESTS_SRC = FileList['tests/*.erl','tests/*.hrl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
TEST_OBJ = TESTS_SRC.pathmap("%{tests,tests_ebin}X.beam")

directory BIN
directory TEST_BIN

namespace :erlang do
    desc "starting qerl"
    task :run => [:compile] do
        sh("erl -noshell -pa #{BIN} -s qerl_example_server start")
    end
end

desc "default set to 'compile'"
task :default => [:compile] do
    sh "cp src/*.app #{BIN}/"
    sh "cp src/*.rel #{BIN}/"
end

desc "start the erlang application"
task :start => ['erlang:run']

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
      sh "erlc -pa #{BIN} -W #{ERLC_FLAGS} -o #{BIN} #{t.source}"
end

rule ".beam" => ["%{tests_ebin,tests}X.erl"] do |t|
      sh "erlc -pa #{TEST_BIN} -W #{ERLC_FLAGS} -o #{TEST_BIN} #{t.source}"
end

desc "compile the current sources"
task :compile => ['ebin'] + OBJ

desc "compile test sources"
task :compile_tests => ['tests_ebin'] + TEST_OBJ

desc "execute eunit tests"
task :test => [:compile,:compile_tests] do
    modules = TEST_OBJ.map{|o| File.basename(o,".beam")}
    output = `erl \
    -noshell \
    -pa tests_ebin -pa ebin \
    -eval 'eunit:test([#{modules.join(",")}], [verbose])' \
    -s init stop`

    output.each_line do |line|
        case line
        when /= (EUnit) =/
            print line.gsub($1, green($1))
        when /\*failed\*/
            print red(line)
        when /(\.\.\..*ok)/
            print line.gsub($1, green($1))
        when /Failed:\s+(\d+)\.\s+Skipped:\s+(\d+)\.\s+Passed:\s+(\d+)\./
            puts "#{red("Failed: #{$1}")} Skipped: #{$2} #{green("Passed: #{$3}")}"
        when /(All \d+ tests passed.)/
            print green(line)
        else
            print line
        end
    end
end

def green(text)
    "\e[32m#{text}\e[0m"
end

def red(text)
    "\e[31m#{text}\e[0m"
end

