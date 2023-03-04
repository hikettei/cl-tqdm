# cl-tqdm
Simple and Fast Progress Bar Library for Common Lisp, inspired in tqdm.

(Optimizing hasn't done yet.)

![demo](https://gyazo.com/5bbc43310df9281c4711446ac7bb23b3/raw)

(Currently estimation of remaining time does not work well lol. Pull requests are welcome I got tired.)

This is the reimplementation of my old library [cl-cram](https://github.com/hikettei/cl-cram) because its APIs aren't useful and there is room to optimize.

## Run Test

```lisp
$ sbcl --script test.lisp
```

## Install

Coming soon... (I've not registered it in quicklisp yet...)

## Features

The basic usage is that: initialize `tqdmbar` structure with `(with-tqdm)` macro and then, call `(update)` function to increate the bar.

`tqdmbar` structure can be initialized with `(tqdm count)` constructor.

```lisp
(with-tqdm x 100 "No info"
  ; print-object will display progress bar but don't incf it.
  (print x)
  ;0% |          | 0/100 [0.0s<0.0s, 0.0s/it]
  (update x :incf 2 :description "Hello world!" :stream t)
  ;Hello world!:  2% |          | 2/100 [0.0s<0.0s, 0.0s/it]
  )
```

If you don't want to use animated progress-bar in specific environment, `(with-config)` macro is available to solve this problem.

This will make it possible to use cl-tqdm in SLIME REPL! ٩(๑>∀<๑)۶

```lisp
(with-config (config :animation nil)
  (with-tqdm x 10 ""
    (dotimes (i 10)
      (update x))))
;  10% |█         | 1/10 [0.0s<0.0s, 0.0s/it]
;  20% |██        | 2/10 [0.0s<0.0s, 0.0s/it]
;  30% |███       | 3/10 [0.0s<0.0s, 0.0s/it]
;  40% |████      | 4/10 [0.0s<0.0s, 0.0s/it]
;  50% |█████     | 5/10 [0.0s<0.0s, 0.0s/it]
;  60% |██████    | 6/10 [0.0s<0.0s, 0.0s/it]
;  70% |███████   | 7/10 [0.0s<0.0s, 0.0s/it]
;  80% |████████  | 8/10 [0.0s<0.0s, 0.0s/it]
;  90% |█████████ | 9/10 [0.0s<0.0s, 0.0s/it]
; 100% |██████████| 10/10 [0.0s<0.0s, 0.0s/it]
```
## Documents

Coming soon...

But the whole code is very simple, so if you want to know the usage, it is the best way to read the source code `./cl-tqdm.lisp`

# Pull Requests

Pull Requests are welcome at original repository [Here](https://github.com/hikettei/cl-tqdm)

# LICENCE

This package is under MIT Licence. See `./LICENCE`

# Workloads

- Fix: Estimating of resting time.
- Todo: Optimize the whole codes
- Todo: Prepare Example Code and Documentations

# Author

hikettei (Twitter: [@ichndm](https://twitter.com/ichndm))

