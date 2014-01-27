# Finalizers for Emacs Lisp

This package provides finalizers for Emacs Lisp objects. Objects
registered with this package will have a specified finalizer function
run *immediately* after that object is garbage collected.

The API has one function: `finalize-register`. It accepts an object, a
finalizer, and arguments to be passed to the finalizer. The object
being finalized will be unavailable to the finalizer.

This package works by taking advantage of weak references and
`post-gc-hook`.

See also: [Emacs Lisp Object Finalizers][blog]

## Usage

Use `delete-process` as a finalizer to clean up a leftover process.

```el
(require 'cl-lib)
(require 'finalize)

(cl-defstruct (pinger (:constructor pinger--create))
  process host)

(defun pinger-create (host)
  (let* ((process (start-process "pinger" nil "ping" host))
         (object (pinger--create :process process :host host)))
    (finalize-register object #'delete-process process)
    object))

(setf pinger (pinger-create "localhost"))
;; => [cl-struct-pinger #<process pinger> "localhost"]

(get-process "pinger")
;; => #<process pinger>

;; Allow object to be garbage collected.
(setf pinger nil)
(garbage-collect)

;; Process has been automatically cleaned up by the finalizer.
(get-process "pinger")
;; => nil
```

Or using the `finalizable` EIEIO mixin class, which calls `finalize`
on a copy of the original object after garbage collection.

```el
(require 'eieio)
(require 'finalizable)

(defclass pinger (finalizable)
  ((process :initarg :process :reader pinger-process)
   (host :initarg :host :reader pinger-host)))

(defun pinger-create (host)
  (make-instance 'pinger
                 :process (start-process "ping" nil "ping" host)
                 :host host))

(defmethod finalize ((pinger pinger))
  (delete-process (pinger-process pinger)))
```

## Closure Caveat

Be mindful when using lexical scope and passing a lambda to
`finalize-register`. Uncompiled lambdas capture their *entire*
environment, which almost certainly
[includes the object subject to finalization][closure]. This will
backfire and keep the object alive indefinitely. This situation will
only work correctly when your function is byte-compiled, which will
provide precise lexical environment capture.


[closure]: http://nullprogram.com/blog/2013/12/30/#the_readable_closures_catch
[blog]: http://nullprogram.com/blog/2014/01/27/
