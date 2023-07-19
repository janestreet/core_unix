module type S = Epoll_intf.S

module Real_impl : S
module Null_impl : S
