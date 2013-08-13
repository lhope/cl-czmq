# cl-czmq - A re-binding of the C binding for the zmq transport layer (czmq). #

## Overview ##

cl-czmq is a lisp binding for the [CZMQ C
binding](http://czmq.zeromq.org/) for the [ZMQ transport
layer](http://zeromq.org/).

ZMQ is an extremely useful socket and messaging layer which makes
network programming not just possible, but fun. [Read the
Guide!](http://zguide.zeromq.org/page:all)

Why CZMQ and not ZMQ?

- Target multiple zmq versions through CZMQ.
- CZMQ is cleaner to implement. There's no need to build an arbitrary
  additional API on top of it - unlike zmq. This means a straight port
  is actually usable.
- A straight port makes it easier to translate between C and lisp for
  e.g. guide examples and documentation.
- CZMQ contains cool constructs such as zbeacon, zloop, zmsg, etc.
- CZMQ is in C not C++ :-)

## Scope and Goals ##

The scope of cl-czmq is to provide CZMQ's functionality to Lisp. CZMQ
provides easy functions to send and receive memory blocks, strings,
message frames, and multi-part messages. It simplifies greatly the ZMQ
socket options.

Some functionality of CZMQ is not ported. Zero copy methods are
pointless, since we have to copy data into C anyway. zthread, zclock,
zlist and zhash aren't ported, as their functionality is provided
adequately by Lisp.

A minimum of extra functionality is introduced See **Using cl-czmq**
below.

The goal is to track a well-used API which provides solid
interoperability with primitive Lisp types. Experience shows that a
"Lispy" CLOS system can be beautiful to some and a hindrance to
others.  Others are free to layer their own API if they like.

## Using cl-czmq ##

cl-czmq is more or less a straight port of czmq. This means the CZMQ
documentation straightforwardly applies. We have, however, made some
changes to facilitate lisp programming style.

- Code is available in the package named `:cl-czmq` (nicknamed `:czmq`).

- Names are dashed instead of underscored. e.g. `zsocket-create`
  instead of `zsocket_create`.

- Constants are in lisp idiomatic style. e.g. `+czmq-version-major+` instead of
  CZMQ_VERSION_MAJOR.

- Where a CZMQ function would return a failure signal, cl-czmq returns
  nil instead. e.g. `NULL` for a pointer or `rc = -1` becomes nil.

- Where a destructor would take a reference and set the destroyed
  variable to `NULL`, we instead return the value, which would be nil
  if the destruction was successful.

- We have introduced a set of `zpollset` functions. These manipulate
  `zmg_pollitem_t` arrays for polling and zloop.

- We have included helper functions and macros to create callback
  functions for zloop. See **zloop and polling** below.

- We have included the common `with-` idiom for some of the
  classes. e.g. `with-zctx`, `with-zsocket`, etc... these take care of
  binding, creating and releasing the underlying blocks of memory.

## polling and zloop

The one place CZMQ is lacking is that it does not provide a polling
class of its own. It uses ZMQ's zmq_pollitem_t. To make up for this,
we have implemented a zpollset class using the same style as CZMQ. See
`src/zpollset.lisp` for documentation.

zloop is a an [event
reactor](http://en.wikipedia.org/wiki/Reactor_pattern) which uses
handler functions to deal with events.

Passing a function to another
is a common idiom in lisp, however `zloop-poller` and `zloop-timer`
require functions with a specific signature. Use `def-zloop-fn` to
create these functions.

Additionally, if you want to pass a lisp object to `zloop-poller` or `zloop-timer`, you need to bind it using `with-zloop-args` or `with-zloop`. This creates a specially crafted c-pointer which `def-zloop-fn` interprets correctly.

## Dependencies

cl-czmq requires libzmq, libczmq, and cffi.

- ZMQ dependencies are available through zeromq.org.

- Lisp dependencies are available through
  [quicklisp](http://www.quicklisp.org).

## Installation

Build zmq and czmq in your platform specific way.

We suggest using [quicklisp](http://www.quicklisp.org). Install that,
and then grab the source (via tarball or git) and put it in
`~/quicklisp/local-projects/`. Symlinks work okay too.

## Documentation

See the czmq(7) manpage. Also see czmq.zeromq.com.

## Ownership and License ##

cl-czmq's contributors are listed in the `AUTHORS` file. The authors of cl-czmq grant you use of this software under the terms of the Lisp Lesser General Public License (LLGPL). For details see the files `COPYING`, `COPYING.LESSER` and `COPYING.LISP` in this directory.

## Contributing ##

If you find problems with this library please take the time to report the issue. This helps you by increasing the chance the issue will be fixed. It helps motivate me by letting me know the library is being used. It helps everyone when the fixed issue creates a stronger codebase.

To report an issue, use the [cl-czmq issue tracker](https://github.com/lhope/cl-czmq/issues) at github.com.

For issue fixes and improvements, we prefer github pull requests.
