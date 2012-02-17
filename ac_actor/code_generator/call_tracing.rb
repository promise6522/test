require "src/spec_data.rb"
require "src/code_writer.rb"

class Step
  attr_reader :actor, :call, :recurse, :fail, :subs, :parent

  def initialize(parent, actor, call)
    @parent = parent
    @actor = actor
    @call = call
    @recurse = false
    @fail = ""
    @subs = []

    if @parent != nil
      @parent.add(self)
    end
  end

  def equals?(step)
    return @actor == step.actor && @call == step.call
  end

  def recursed
    @recurse = true
  end

  def failed(msg)
    @fail = msg
  end

  def add(sub)
    subs << sub
  end
end

class CallTracer
  attr_reader :core_data, :traces

  def initialize(core)
    @core_data = core
    @traces = Step.new(nil, "top", "top")

    @core_data.actors.values.each {
      |a|
      a.calls.values.each {
        |c|
        trace(c, traces)
      }
    }
  end

  def trace(c, step)
    ns = Step.new(step, c.actor_name, c.name)

    p = ns
    while p = p.parent do
      if p.equals?(ns)
        ns.recursed
        return
      end
    end

    o = c.call_outs
    if o.length < 1
      return
    end

    o.each {
      |out|
      act = @core_data.actors[out.target_type]
      if act == nil
        bad = Step.new(ns, out.target_type, out.call_name)
        bad.failed("Actor not found: " + out.target_type)
      elsif act.calls[out.call_name] == nil
        bad = Step.new(ns, out.target_type, out.call_name)
        bad.failed("Call not found in actor " + out.target_type + ": " + out.call_name)
      else
        trace(act.calls[out.call_name], ns)
      end
    }
  end

  def print_traces(w)
    print_recursive(w, traces, 0)
  end

  def print_recursive(w, s, layer)
    w.println(layer.to_s + " : " + s.actor + "/" + s.call)
    if s.fail != ""
      w.println("XXXXXXXXX " + s.fail)
    elsif s.recurse
      w.println("========= Recursion here")
    end
    s.subs.each {
      |sub|
      w.indent
      print_recursive(w, sub, layer+1)
      w.dedent
    }
  end
end

if __FILE__ == $0
  o = Core.new("./res")
  o.print_error("core")
  x = CallTracer.new(o)
  x.print_traces(CodeWriter.new($stdout))
end
