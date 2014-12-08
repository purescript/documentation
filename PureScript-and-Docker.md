# Dependencies

First you'll need a docker install: https://docs.docker.com/installation/

# psci

After installing Docker, we'll run a PureScript psci session by running the 0.6.1.1 image. The `-it` flags allocate a pseudo tty and make it interactive. We're pulling the image from the default docker hub, so the images are [here](https://registry.hub.docker.com/u/biscarch/purescript/) and the code that builds them is [on GitHub](https://github.com/ChristopherBiscardi/purescript)

```
docker run -it biscarch/purescript:0.6.1.1
```

We should see something like this as a result (if you have not previously downloaded the image):

```
Unable to find image 'biscarch/purescript:0.6.1.1' locally
Pulling repository biscarch/purescript
85cf1246cf87: Download complete
511136ea3c5a: Download complete
848d84b4b2ab: Download complete
71d9d77ae89e: Download complete
9c7db00d9eab: Download complete
e512573c88ba: Download complete
2196d28d9b54: Download complete
05d488a9d6e4: Download complete
4ef15268a341: Download complete
c1b3e4601eb0: Download complete
78fc43069ca8: Download complete
780cac993ca9: Download complete
195e5a88f40e: Download complete
841f3b11dcbe: Download complete
b3543e32d6e3: Download complete
fcdd94c29651: Download complete
153ca9b43caf: Download complete
Status: Downloaded newer image for biscarch/purescript:0.6.1.1
 ____                 ____            _       _
|  _ \ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_
| |_) | | | | '__/ _ \___ \ / __| '__| | '_ \| __|
|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_
|_|    \__,_|_|  \___|____/ \___|_|  |_| .__/ \__|
                                       |_|

:? shows help

Expressions are terminated using Ctrl+D
>
```

Cool, a `psci`. Let's test it out:

```
> :t 5
Compiling Prelude
Compiling Prelude.Unsafe
Compiling Data.Function
Compiling Data.Eq
Compiling Control.Monad.Eff
Compiling Control.Monad.Eff.Unsafe
Compiling Control.Monad.ST
Compiling Debug.Trace
Prim.Number
```

5 is a Number, which is what we expected.

We can kill the repl (and container) as we normally would exit a `psci` (with `:q`)

# What's inside

Currently all the purescript binaries are in `/opt/purescript`. They are also on the PATH.

```
> ls /opt/purescript
LICENSE
README
psc
psc-docs
psc-make
psci
```

In addition to the purescript binaries, `node` and `npm` are installed and on the path.

```
> node -v
v0.10.32
> npm -v
2.1.4
```

# Starting a Project

Let's start a new project with [grunt-init-purescript](https://github.com/purescript-contrib/grunt-init-purescript). We can take two approaches to this (or a combination thereof). For now, we'll use the base image without writing a `Dockerfile`.

Make a new directory on the host:

```
mkdir my-new-project
cd my-new-project
```

Run a new biscarch/purescript container. We'll mount our current directory inside the container so that any changes we make persist when the container dies.

```
docker run -itv `pwd`:/files biscarch/purescript bash
```

Now that we're in the container (with bash), let's start a new purescript project as normal according to [grunt-init-purescript](). Note that we don't have git installed in the container, so we'll have to install it first.

```
apt-get install git -y
npm install -g grunt grunt-init grunt-cli bower
mkdir ~/.grunt-init
git clone https://github.com/purescript-contrib/grunt-init-purescript.git ~/.grunt-init/purescript
cd /files
grunt-init purescript
```

`grunt-init purescript` will give output that looks like below, feel free to modify any of the fields.

```
grunt-init purescript
Running "init:purescript" (init) task
This task will create one or more files in the current directory, based on the
environment and the answers to a few questions. Note that answering "?" to any
question will show question-specific help and answering "none" to most questions
will leave its value blank.

Please answer the following:
[?] Project name (starter-kit)
[?] Description (An empty PureScript project.)
[?] Version (0.1.0)
[?] Licenses (MIT)
[?] Author name (none)
[?] Do you need to make any changes to the above before continuing? (y/N) N

Writing .bowerrc...OK
Writing .gitignore...OK
Writing Gruntfile.js...OK
Writing README.md...OK
Writing bower.json...OK
Writing js/index.js...OK
Writing package.json...OK
Writing src/Starter/Kit/Example.purs...OK
Writing tests/Tests.purs...OK
Writing LICENSE-MIT...OK

Initialized from template "purescript".
You can now build the project as follows:

  npm install
  bower update
  grunt

Done, without errors.
```

Now that we've initialized a project, lets install dependencies and compile the project using grunt:

```
npm install
bower update
grunt
```

That's it. We can now check that the files were actually persisted to the host by exiting the container:

```
$ exit
exit
> ls
Gruntfile.js     LICENSE-MIT      README.md        bower.json       bower_components js
node_modules     output           package.json     src              tests            tmp
```

## Dockerfile

There's a slight problem with this though. We won't have bower, etc installed when we try to run the container again! This is an easy fix, we'll create a `Dockerfile` in the root of our project.

```
# my-new-project/Dockerfile
FROM biscarch/purescript:0.6.1.1

RUN npm install grunt grunt-cli bower
```