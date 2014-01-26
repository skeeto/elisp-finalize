# Finalizers for Emacs Lisp

This package provides finalizers for Emacs Lisp objects. Objects
registered with this package will have a specified finalizer function
run *immediately* after that object is garbage collected.

The API has one function: `finalize-register`. It accepts an object, a
finalizer, and arguments to be passed to the finalizer. The object
being finalized will be unavailable to the finalizer.

This package works by taking advantage of weak references and
`post-gc-hook`.

## Usage

```el
;; Side-effect global variable.
(setf result nil)

;; Register a fresh list and let it go.
(finalize-register (list 1 2 3)
                   (lambda () (setf result :done)))

result  ; => nil
(garbage-collect)
result  ; => :done
```

Here's a more practical example, using a finalizer to clean up a
leftover process.

```el
(require 'cl-lib)
(require 'finalize)

(cl-defstruct (pinger (:constructor pinger--create))
  process host)

(defun pinger-create (host)
  (let* ((process (start-process "pinger" nil "ping" host))
         (object (pinger--create :process process :host host)))
    (finalize-register object #'kill-process process)
    object))

(setf pinger (pinger-create "localhost"))
;; => [cl-struct-pinger #<process pinger<1>> "localhost"]

(get-process "pinger")
;; => #<process pinger>

;; allow object to be garbage collected
(setf pinger nil)
(garbage-collect)

;; Process has been automatically cleaned up by finalizer.
(get-process "pinger")
;; => nil
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
