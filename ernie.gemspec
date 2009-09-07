# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{ernie}
  s.version = "0.3.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Tom Preston-Werner"]
  s.date = %q{2009-09-06}
  s.default_executable = %q{ernie}
  s.email = %q{tom@mojombo.com}
  s.executables = ["ernie"]
  s.extensions = ["ext/extconf.rb"]
  s.extra_rdoc_files = [
    "LICENSE",
     "README.md"
  ]
  s.files = [
    ".document",
     ".gitignore",
     "LICENSE",
     "README.md",
     "Rakefile",
     "VERSION.yml",
     "bin/ernie",
     "ebin/ernie_server_app.app",
     "elib/asset_pool.erl",
     "elib/asset_pool_sup.erl",
     "elib/ernie_server.erl",
     "elib/ernie_server_app.erl",
     "elib/ernie_server_sup.erl",
     "elib/logger.erl",
     "elib/logger_sup.erl",
     "elib/port_wrapper.erl",
     "ernie.gemspec",
     "examples/calc.rb",
     "ext/Makefile",
     "ext/extconf.rb",
     "lib/ernie.rb",
     "test/ernie_test.rb",
     "test/load.rb",
     "test/test_helper.rb"
  ]
  s.homepage = %q{http://github.com/mojombo/ernie}
  s.rdoc_options = ["--charset=UTF-8"]
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.5}
  s.summary = %q{Ernie is a BERT-RPC server implementation.}
  s.test_files = [
    "test/ernie_test.rb",
     "test/load.rb",
     "test/test_helper.rb",
     "examples/calc.rb"
  ]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<erlectricity>, [">= 1.0.1"])
      s.add_runtime_dependency(%q<bertrpc>, [">= 0.2.0"])
    else
      s.add_dependency(%q<erlectricity>, [">= 1.0.1"])
      s.add_dependency(%q<bertrpc>, [">= 0.2.0"])
    end
  else
    s.add_dependency(%q<erlectricity>, [">= 1.0.1"])
    s.add_dependency(%q<bertrpc>, [">= 0.2.0"])
  end
end
