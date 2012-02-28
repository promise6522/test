require "src/spec_data.rb"
require "src/code_writer.rb"
require "src/call_tracing.rb"
require "fileutils"

class DocWriter
  def initialize(core, dest)
    @core = core
    @dest = dest
  end

  def print_all()
    ["style.css", "index.html", "header.html", "banner.jpg"].each {
      |f|
      FileUtils.cp("src/res/" + f, "doc/" + f)
    }

    write_top_list
    write_types_list
    @core.actors.values.each {|a|  write_actor(a)}
    @core.types.values.each {|t|  write_type(t)}
    $built_literal_type.each {|t|  write_builtin_type(t)}
    $built_id_type.each {|t|  write_builtin_type(t)}
  end

  def write_top_list()
    p = SimplePage.new("Top list")
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add("Actors")
    @core.actors.values.each {
      |a|
      p.body.add(actor_link(a.name, a.name))
      p.body.add("<br/>")
    }

    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add("Others")
    p.body.add(write_link("types", "type_list.html", "actor_pane"))
    p.body.add("<br/>")

    p.body.add(write_link("top level errors", "errors_top.html", "details_pane"))
    p.body.add("<br/>")

    p.dumpit(@dest + "/actors.html")

    write_errors(@core, "/errors_top.html")
  end

  def write_types_list()
    p = SimplePage.new("Types list")
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add("Types")
    fn = "../../../src/core/include/ac_message_type.h"
    title.add(write_link("Source", fn, "details_pane"))

    @core.types.values.each {
      |t|
      p.body.add(type_link(t.name, t.name))
      p.body.add("<br/>")
    }
    p.dumpit(@dest + "/type_list.html")
  end

  def write_errors(el, f)
    p = SimplePage.new("Errors from "  + el.name)
    p.add_css("style.css")
    l = p.body.new_element("p")
    l.add_attr("class", "sectiontitle")
    l.add("Errors from " + el.name)
    if el.errors.length == 0
      l = p.body.new_element("p")
      l.add_attr("class", "text")
      l.add("No errors found here")
    else
      el.errors.each {
        |s|
        l = p.body().new_element("p")
        l.add_attr("class", "text")
        l.add(s)
      }
    end
    p.dumpit(@dest + f)
  end

  def write_actor(a)
    fn = "Actor " + a.name
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)
    if a.is_singleton
      title.add("&nbsp;&nbsp;[singleton]")
    end
    title.add("<br/>")
    fn = "../../../../src/core/actor/include/" + a.name + ".h"
    title.add(write_link("Actor source", fn, "details_pane"))
    fn = "../../../../src/core/actor/include/" + a.name + "_helper.h"
    title.add(write_link("Helper source", fn, "details_pane"))

    if a.errors.length > 0
      fn = "error_actor_" + a.name + ".html"
      p.body.add(write_link("Errors", fn, "details_pane"))
      write_errors(a, "/" + fn)
    end

    init_count = 0;
    a.inits.each {
      |c|
      title = p.body.new_element("p")
      title.add_attr("class", "sectiontitle")

      if c.async
        title.add("Init [async]")
      else
        title.add("Init [sync]")
      end

      fn = a.name + "_init" + "_" + init_count.to_s
      p.body.add(xml_link("Xml", fn))
      p.body.add("<br/>")
      fn = "xml_" + fn + ".html"
      write_call_xml(c, fn)

      if c.errors.length > 0
        fn = "error_actor_" + a.name + "_call_" + c.name + ".html"
        p.body.add(write_link("Errors", fn, "details_pane"))
        write_errors(c, "/" + fn)
      end

      fn = "actor_" + a.name + "_init_" + init_count.to_s + "_trace.html"
      p.body.add(write_link("Init&nbsp;trace", fn, "details_pane"))
      write_call_trace(c, fn)

      p.body.add("<br/>");
      fn = "actor_" + a.name + "_init_"+ init_count.to_s + "_pseudo.html"
      p.body.add(write_link("Pseudo&nbsp;Code", fn, "details_pane"))
      write_pseudo_code(c, fn)
      init_count += 1
    }

    a.calls.values.each {
      |c|
      title = p.body.new_element("p")
      title.add_attr("class", "sectiontitle")

      if c.async
        title.add("Call " + c.name + " [async]")
      else
        title.add("Call " + c.name + " [sync]")
      end

      fn = a.name + "_" + c.name
      p.body.add(xml_link("Xml", fn))
      p.body.add("<br/>")
      fn = "xml_" + a.name + "_" + c.name + ".html"
      write_call_xml(c, fn)

      p.body.add(type_link("Input", c.input_type))
      p.body.add(type_link("Output", c.output_type))
      if c.errors.length > 0
        fn = "error_actor_" + a.name + "_call_" + c.name + ".html"
        p.body.add(write_link("Errors", fn, "details_pane"))
        write_errors(c, "/" + fn)
      end

      p.body.add("<br/>");
      fn = "actor_" + a.name + "_" + c.name + "_trace.html"
      p.body.add(write_link("Call&nbsp;trace", fn, "details_pane"))
      write_call_trace(c, fn)

      p.body.add("<br/>");
      fn = "actor_" + a.name + "_" + c.name + "_pseudo.html"
      p.body.add(write_link("Pseudo&nbsp;Code", fn, "details_pane"))
      write_pseudo_code(c, fn)

      p.body.add("<br/>");
      fn = "actor_" + a.name + "_" + c.name + "_caller_list.html"
      if(write_caller_list(c, fn))
        p.body.add(write_link("Caller&nbsp;List", fn, "details_pane"))
      else
        p.body.add(write_red_link("Caller&nbsp;List", fn, "details_pane"))
      end
    }
    p.dumpit(@dest + "/actor_" + a.name + ".html")
  end

  def write_call_trace(c, ff)
    fn = "Call trace " + c.actor_name + " [" + c.name + "]"
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)

    p.body.add(actor_link("Host actor", c.actor_name))

    top = Step.new(nil, "top", "top")
    tracer = CallTracer.new(@core)
    tracer.trace(c, top)
    print_trace(p.body, "", top, 0)

    p.dumpit(@dest + "/" + ff)
  end

  def write_caller_list(call, ff)
    fn = "Caller List for " + call.actor_name + " [" + call.name + "]"
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)

    isfind = false
    @core.actors.each {
      |k1, a|
      a.calls.each {
        |k2, c|
        c.call_outs.each {
          |co|
          if co.target_type == call.actor_name && co.call_name == call.name
            l = p.body.new_element("span")
            l.add_attr("class", "text")
            l.add(actor_link(c.actor_name, c.actor_name))
            l.add("." + c.name + "<br/>")
            isfind = true
          end
        }
      }

      a.inits.each {
        |c|
        c.call_outs.each {
          |co|
          if co.target_type == call.actor_name && co.call_name == call.name
            l = p.body.new_element("span")
            l.add_attr("class", "text")
            l.add(actor_link(c.actor_name, c.actor_name))
            l.add("." + c.name + "<br/>")
            isfind = true
          end
        }
      }
    }
    p.dumpit(@dest + "/" + ff)

    return isfind
  end

  def write_call_xml(call, ff)
    fn = "Call XML for " + call.actor_name + " [" + call.name + "]"
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)

    p.body.add("<pre>")
    call_string = "&lt;call name=\"" + call.name + "\""
    if not call.input_type.empty?
      call_string += " input_type=\"" + call.input_type + "\""
    end
    if (call.name != "init") and (not call.output_type.empty?)
      call_string += " output_type=\"" + call.output_type + "\""
    end
    call_string += " async=\"" + call.async.to_s + "\""
    if call.name != "init"
      call_string += " short_response=\"" + call.short_response.to_s + "\""
    end
    call_string += " />"

    p.body.add(call_string)
    call.call_outs.each {
      |cs|
      callout_string = "  &lt;call_out target_type=\"" + cs.target_type + "\" call_name=\"" + cs.call_name + "\" />"
      p.body.add(callout_string)
    }

    p.body.add("</pre>")

    p.dumpit(@dest + "/" + ff)
  end

  def write_pseudo_code(c, ff)
    fn = "Pseudo code for " + c.actor_name + " [" + c.name + "]"
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)

    c.pseudo_codes.each {
      |e|
      p.body.add("<pre>")
      p.body.add(e.pseudo_code)
      p.body.add("</pre>")
    }

    p.dumpit(@dest + "/" + ff)
  end

 def write_builtin_type(t)
    fn = "builtin type :" + t
    p = SimplePage.new(fn)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add(fn)
    p.dumpit(@dest + "/type_" + t + ".html")
  end

  def print_trace(x, pre, c, layer)
    l = x.new_element("span")
    l.add_attr("class", "text")
    l.add(pre)
    l.add(layer.to_s + "&nbsp;:&nbsp;")
    l.add(actor_link(c.actor, c.actor))
    l.add("." + c.call + "<br/>")
    if c.recurse
      l.add(pre + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(recursion here)<br/>")
      return
    end
    c.subs.each {
      |sub|
      print_trace(x, pre + "&nbsp;&nbsp;&nbsp;&nbsp;", sub, layer+1)
    }
  end

  def type_link(title, name)
    if name == ''
      return ""
    end

    if @core.types[name] == nil and not @core.is_built_type(name)
      d = XElement.new("span")
      d.add_attr("class","linkind")
      d.add_attr("style","color:red")
      d.add(title)
      return d
    end

    return write_link(title, "type_" + name + ".html", "details_pane")
  end

  def actor_link(title, name)
    if name == ''
      return ""
    end

    if @core.actors[name] == nil and not @core.is_built_type(name)
      d = XElement.new("span")
      d.add_attr("class","linkind")
      d.add_attr("style","color:red")
      d.add(title)
      return d
    end

    return write_link(title, "actor_" + name + ".html", "actor_pane")
  end

  def xml_link(title, name)
    if name == ''
      return ""
    end

    return write_link(title, "xml_" + name + ".html", "details_pane")
  end

  def write_type(t)
    p = SimplePage.new("Type " + t.name)
    p.add_css("style.css")
    title = p.body.new_element("p")
    title.add_attr("class", "sectiontitle")
    title.add("Type " + t.name)

    t.fields.values.each {
      |f|
      if f.tag_name == "sub"
        p.body.add(type_link(f.type_name, f.type_name))
      elsif f.tag_name == "id"
        p.body.add(actor_link("Id (of " + f.type_name + ")", f.type_name))
      else
        p.body.add('<span class="linkind">' + f.type_name + "</span>")
      end
      if f.is_array
        p.body.add("[]")
      end
      p.body.add("&nbsp;&nbsp;&nbsp;" + f.name + "<br/>")
    }
    p.dumpit(@dest + "/type_" + t.name + ".html")
  end

  def write_link(text, href, target)
    l = XElement.new("a")
    l.add_attr("href", href)
    l.add_attr("target", target)
    l.add_attr("class", "linkind")
    l.add(text)
    return l
  end

  def write_red_link(text, href, target)
    l = XElement.new("a")
    l.add_attr("href", href)
    l.add_attr("target", target)
    l.add_attr("class", "linkind")
    l.add_attr("style", "color:red")
    l.add(text)
    return l
  end
end

if __FILE__ == $0

  o = Core.new("./res")

  if not File.exist?("./doc")
    FileUtils.mkdir("./doc")
  end
  d = DocWriter.new(o, "./doc")
  d.print_all()
end
