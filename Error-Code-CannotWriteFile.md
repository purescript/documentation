#### 2016-11-29 20:32 AEST
This happened to me (different person) and I closed Atom which was open to the project folder and it solved it. Opened atom again and problem was back again. So check what might have a lock on the directory. Sucks to need to close the editor but its a workaround.

#### 2016-08-19 15:37 AEST
Actually this just happened to me again (in a different directory), when trying to evaluate `ints` inside psci with a Main that contained `ints = [1, 3, 5, 7, 11, 13, 17, 23]`.  Again, it worked fine the second time.  So it's not related to anything specific in Global...

#### 2016-08-19 09:30 AEST
I was working on Purescript by Example Ex 2.2 and did `bower install purescript-globals -S` and then the following, where the problem came the *first* time I tried `isNaN infinity`.  But the second time I tried it worked fine.

```
castor ~/purs/by-example/% pulp psci
pulp psci
Compiling Global.Unsafe
Compiling Global
PSCi, version 0.9.3
Type :? for help

> import Global
import Global

> isNaN 44.0
isNaN 44.0
false

> isNaN nan
isNaN nan
true

> isNaN infinity
isNaN infinity
Error found:

  Unable to write file:

    .psci_modules/node_modules/$PSCI/index.js

See https://github.com/purescript/purescript/wiki/Error-Code-CannotWriteFile for more information,
or to contribute content related to this error.

> :?
:?
The following commands are available:
<elided for brevity>

> isNaN infinity
isNaN infinity
false
```

The index file is there now, after the second try:

```
% ls ~/purs/by-example/.psci_modules/node_modules/\$PSCI/index.js 
/home/gtod/purs/by-example/.psci_modules/node_modules/$PSCI/index.js
```
