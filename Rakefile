require 'rubygems'
require 'rake'

begin
  require 'jeweler'
  Jeweler::Tasks.new do |gem|
    gem.name = "ernie"
    gem.rubyforge_project = "ernie"
    gem.summary = %Q{Ernie is a BERT-RPC server implementation.}
    gem.description = %Q{Ernie is an Erlang/Ruby hybrid BERT-RPC server implementation packaged as a gem.}
    gem.email = "tom@mojombo.com"
    gem.homepage = "http://github.com/mojombo/ernie"
    gem.authors = ["Tom Preston-Werner"]
    gem.files.include(["ext"])
    gem.extensions << 'ext/extconf.rb'
    gem.add_dependency('bert', '>= 1.1.0')
    gem.add_dependency('bertrpc', '>= 1.0.0')

    # gem is a Gem::Specification... see http://www.rubygems.org/read/chapter/20 for additional settings
  end
rescue LoadError
  puts "Jeweler not available. Install it with: sudo gem install technicalpickles-jeweler -s http://gems.github.com"
end

require 'rake/testtask'
Rake::TestTask.new(:test) do |test|
  test.libs << 'lib' << 'test'
  test.pattern = 'test/**/*_test.rb'
  test.verbose = true
end

begin
  require 'rcov/rcovtask'
  Rcov::RcovTask.new do |test|
    test.libs << 'test'
    test.pattern = 'test/**/*_test.rb'
    test.verbose = true
  end
rescue LoadError
  task :rcov do
    abort "RCov is not available. In order to run rcov, you must: sudo gem install spicycode-rcov"
  end
end

task :default => :test

# require 'rake/rdoctask'
# Rake::RDocTask.new do |rdoc|
#   if File.exist?('VERSION.yml')
#     config = YAML.load(File.read('VERSION.yml'))
#     version = "#{config[:major]}.#{config[:minor]}.#{config[:patch]}"
#   else
#     version = ""
#   end
# 
#   rdoc.rdoc_dir = 'rdoc'
#   rdoc.title = "ernie #{version}"
#   rdoc.rdoc_files.include('README*')
#   rdoc.rdoc_files.include('lib/**/*.rb')
# end

task :ebuild do
  ERLC_TEST_FLAGS = ""
  ERLC_FLAGS = "-o ../ebin"
  cd "elib"
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} #{Dir["**/*.erl"].join(" ")}"
end