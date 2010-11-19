Gem::Specification.new do |s|
  s.specification_version = 2 if s.respond_to? :specification_version=
  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.rubygems_version = '1.3.6'

  s.name              = 'ernie'
  s.version           = '2.4.0'
  s.date              = '2010-11-19'
  s.rubyforge_project = 'ernie'

  s.summary     = "Ernie is a BERT-RPC server implementation."
  s.description = "Ernie is an Erlang/Ruby hybrid BERT-RPC server implementation packaged as a gem."

  s.authors  = ["Tom Preston-Werner"]
  s.email    = 'tom@mojombo.com'
  s.homepage = 'http://github.com/mojombo/ernie'

  s.require_paths = %w[lib]

  s.extensions = ["ext/extconf.rb", "ext/extconf.rb"]

  s.executables = ["ernie"]
  s.default_executable = 'ernie'

  s.rdoc_options = ["--charset=UTF-8"]
  s.extra_rdoc_files = %w[LICENSE README.md]

  s.add_dependency('bert', [">= 1.1.0"])
  s.add_dependency('bertrpc', [">= 1.0.0"])

  s.add_development_dependency('shoulda', [">= 2.11.3", "< 3.0.0"])

  # = MANIFEST =
  s.files = %w[
    History.txt
    LICENSE
    README.md
    Rakefile
    VERSION.yml
    bin/ernie
    contrib/ebench.erl
    ebin/ernie_server_app.app
    elib/asset_pool.erl
    elib/asset_pool_sup.erl
    elib/bert.erl
    elib/ernie.hrl
    elib/ernie_access_logger.erl
    elib/ernie_access_logger_sup.erl
    elib/ernie_admin.erl
    elib/ernie_config.erl
    elib/ernie_native.erl
    elib/ernie_server.erl
    elib/ernie_server_app.erl
    elib/ernie_server_sup.erl
    elib/logger.erl
    elib/logger_sup.erl
    elib/port_wrapper.erl
    ernie.gemspec
    examples/example.cfg
    examples/example.config
    examples/ext.erl
    examples/ext.rb
    examples/nat.erl
    ext/Makefile
    ext/extconf.rb
    lib/ernie.rb
    test/ernie_server_test.rb
    test/ernie_test.rb
    test/helper.rb
    test/load.rb
    test/sample/ext.rb
    test/sample/intTest.erl
    test/sample/sample.cfg
    test/test_ernie.rb
    test/test_ernie_server.rb
  ]
  # = MANIFEST =

  s.test_files = s.files.select { |path| path =~ /^test\/test_.*\.rb/ }
end
