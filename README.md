# Description
+ Lifegame simulator using hashlife algorithm
+ can read several pattern files ( .rle, .lif, .mc )

![puffer train](https://github.com/glider-gun/hashlife/raw/master/img/puffer-train.png)

# Usage
sbcl with quicklisp is needed.
please install lispbuilder-sdl library beforehand.

```
$ ./hashlife.lisp
```
should launch program and start "puffer train" pattern.

You can use
+ arrow keys : move around
+ ctrl + up/down key : zoom in / out
+ shift + up/down key : speeding up / down
+ space key : pause / start
+ left mouse button : add live cell
+ c : clear
+ f : toggle full screen
+ q : quit

If you have pattern files, please give it as command line argument.
If you have none, try downloading e.g. https://github.com/jimblandy/golly/blob/master/src/Patterns/Life/Breeders/LWSS-breeder.rle and
load it like
```
$ ./hashlife.lisp LWSS-breeder.rle
```

# Todo
+ reduce memory usage
+ resolve slow down problem for very huge pattern (like metapixel-galaxy)
