require 'rubygems'
require 'bert'
require 'logger'

class Ernie
  class << self
    attr_accessor :mods, :current_mod, :logger
  end

  self.mods = {}
  self.current_mod = nil
  self.logger = nil

  def self.mod(name, block)
    m = Mod.new(name)
    self.current_mod = m
    self.mods[name] = m
    block.call
  end

  def self.fun(name, block)
    self.current_mod.fun(name, block)
  end

  def self.logfile(file)
    self.logger = Logger.new(file)
  end

  def self.log(text)
    self.logger.info(text) if self.logger
  end

  def self.dispatch(mod, fun, args)
    xargs = BERT::Decoder.convert(args)
    self.log("-- " + [mod, fun, xargs].inspect)
    self.mods[mod] || raise(ServerError.new("No such module '#{mod}'"))
    self.mods[mod].funs[fun] || raise(ServerError.new("No such function '#{mod}:#{fun}'"))
    res = self.mods[mod].funs[fun].call(*xargs)
    BERT::Encoder.convert(res)
  end

  def self.start
    self.log("Starting")
    self.log(self.mods.inspect)
    receive do |f|
      f.when([:call, Symbol, Symbol, Array]) do |mod, fun, args|
        self.log("-> " + [:call, mod, fun, args].inspect)
        begin
          res = self.dispatch(mod, fun, args)
          xres = [:reply, res]
          self.log("<- " + xres.inspect)
          f.send!(xres)
        rescue ServerError => e
          xres = [:error, [:server, 0, e.class.to_s, e.message, e.backtrace]]
          self.log("<- " + xres.inspect)
          self.log(e.backtrace.join("\n"))
          f.send!(xres)
        rescue Object => e
          xres = [:error, [:user, 0, e.class.to_s, e.message, e.backtrace]]
          self.log("<- " + xres.inspect)
          self.log(e.backtrace.join("\n"))
          f.send!(xres)
        end
        f.receive_loop
      end

      f.when([:cast, Symbol, Symbol, Array]) do |mod, fun, args|
        self.log("-> " + [:cast, mod, fun, args].inspect)
        begin
          self.dispatch(mod, fun, args)
        rescue Object => e
          # ignore
        end
        f.send!([:noreply])
        f.receive_loop
      end

      f.when(Any) do |any|
        self.log("-> " + any.inspect)
        xres = [:error, [:server, 0, "Invalid request: #{any.inspect}"]]
        self.log("<- " + xres.inspect)
        f.send!(xres)
        f.receive_loop
      end
    end
  end
end

class Ernie::ServerError < StandardError; end

class Ernie::Mod
  attr_accessor :name, :funs

  def initialize(name)
    self.name = name
    self.funs = {}
  end

  def fun(name, block)
    self.funs[name] = block
  end
end

# Root level calls

def mod(name, &block)
  Ernie.mod(name, block)
end

def fun(name, &block)
  Ernie.fun(name, block)
end

def logfile(name)
  Ernie.logfile(name)
end

at_exit do
  Ernie.start unless $test
end