param = "old"
a = binding
a.eval("puts param")

def get_binding(param)
  return binding
end
b = get_binding("new")
b.eval("puts param")
