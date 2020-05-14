include
  Expression_intf.Interface
    with type ('t, 'tag) t = ('t, 'tag) Expression_intf.t

include module type of Expression_intf
