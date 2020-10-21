## cl-liballegro
cl-liballegro provides bindings to Allegro5 which already provides [fantastic documentation](https://liballeg.org/a5docs/trunk/)

The documentation that will be provided here is therefore meant for idioms, usages, and general use of these bindings that are more specific to Common Lisp

## Basic usage
The most basic usage is 1-to-1 just uses the bindings "as is", with an example [here](https://github.com/resttime/cl-liballegro/blob/master/examples/simple-window.lisp)

![simple window](https://user-images.githubusercontent.com/2598904/96662425-f3c4cf00-1313-11eb-9e59-807e27697c20.png)

Names have been changed to use a more lispy convention in which `_` is converted to `-`

In this case most function names simply match and there's not much different `al_flip_display(display);` becomes `(al:flip-display display)`

However types, constants, and structures have been shortened for convenience. 
There's no exact rules for it, but usually any `ALLEGRO_*` is truncated.
Any `al_*` is truncated because the `Common Lisp` packages handle the namespace.

An example is `ALLEGRO_KEY_K` is `:k` which may seem drastic at first, but using keywords over constants tends to be convenient in practice becausse CFFI takes care of translating the value to the keyword and vice-versa. For example, a keyboard function in `C` would return a value but the corresponding `Common Lisp` can return a keyword.

## cffi
Occasionally there are times when dropping down to a lower level to using CFFI is required. This happens when it's necesssary to pass a non-opaque data structure by reference.

```c
{
  ALLEGRO_EVENT event;
  bool running = true;
  while (running) process_event(&event);
}
```

In Common Lisp we use CFFI to allocate the structure for the corresponding Allero5 functions. Remember to free up the memory!
```lisp
(defparameter *running-p* t)
(let ((event (cffi:foreign-alloc '(:union al:event)))
  (loop while *running-p* do (process-event event))
  (cffi:foreign-free event))
```
## Orphaned Windows / Cleaning up Windows
At times when someting goes wrong the debugger pops up and a new window is created without the previous one being destroyed. This is due to how debugger restarts execution. One of the ways to handle this is wrapping things in an `unwind-protect` or using the condition handlers in `Common Lisp` to handle errors in such a way that restarts do not re execute certain s-exps to create a new display.

## Lispy Interface
An optional lispy interface is included with cl-liballegro which provides a full game loop with a fixed timestep and Entity-Component-System (ECS) throug the CLOS.

1. Define system
2. Define handlers
3. Run system
