# cvfs todo

* figure out how to do backends that require bigger deps
 - bundle here, don't install dep? chicken /will/ complain.
 - make another egg? probably the sane choice.

# backend/feature ideas

* 9p backend, because Plan 9 did everything* right(TM)!
 - [eggref](http://wiki.call-cc.org/eggref/4/9p), really simple! copy -posix, fix init functions, done. perfect fit for this, actually.
* physfs backend
 - [eggref](http://wiki.call-cc.org/eggref/4/physfs), bit of an effort, but very handy!
 - basically do the same bindings that i've done with carbon's vfs: mount things under /drivename, prevents overlap.
* fuse stuff
 - [eggref](http://wiki.call-cc.org/eggref/4/fuse) doesn't look too hard?
