PureScript Conf is a _free_, one-day conference concentrating on the PureScript language and its libraries.

*PureScript Conf is sponsored by [SlamData](http://slamdata.com), an open source company that builds analytics and reporting software for NoSQL data stores.*

## Where?

PureScript Conf will be colocated with [LambdaConf](http://degoesconsulting.com/lambdaconf-2015).

Address:

1050 Walnut Street  
Suite 202  
Boulder, CO 80302 

**Please do NOT enter the main entrance of Suite 202; look for signs that will direct you to a side entrance.**

## When?

PureScript Conf will be held on Thursday, May 21st 2015, the day before [LambdaConf](http://degoesconsulting.com/lambdaconf-2015).

- Sessions: 9:00 - 5:00 PM
- Lunch: 12:00

## Instructions for Attendees

You might like to install the PureScript compiler in advance, so that you can follow along with the talks and examples:

- Install the PureScript compiler for your operating system. 
  - You should install the [0.6.9.5 release](https://github.com/purescript/purescript/releases/tag/v0.6.9.5). 
  - If you prefer to build from source, be sure to check out the `v0.6.9.5` tag of the [`purescript`](https://github.com/purescript/purescript) compiler repository. The code will **not** build using `master`.
  - Make sure `psc` and `psc-make` are on your PATH.
- Install Node and the NPM package manager
- Install the `pulp` command line tool (`npm install -g pulp`)

## Speakers/Schedule

### Why PureScript?

**@jdegoes**

**10 minutes**

_Abstract_

In this introductory talk, John A. De Goes talks about reasons why you should (or should not) choose PureScript for a new project, and gives a demo of [SlamData](http://slamdata.com), a rich web application built in PureScript (and quite possibly the largest PureScript application ever developed).

### Crash Course in PureScript

**@puffnfresh**

**1 hour**

_Abstract_

An introduction to PureScript, describing the core principles and features:

* Algebraic data types
* Type-classes
* Rank-N types
* Records
* FFI
* Effects
* Tooling

An assumption is made that you're familiar with functional programming. A version of the presentation without that assumption will be given at [BoulderJS](http://www.meetup.com/Boulder-JS/) on Wednesday the 20th (the day before PureScript Conf).

### Principled, Painless Asynchronous Programming in PureScript

**@jdegoes**

**1 hour**

_Abstract_

Asynchronous programming in the browser or node.js is painful, and error-prone! Promises/A+ "promises" to make that better, but the complexity of the numerous ad hoc specifications ensure plenty of confusion, corner cases, and tangled, incomprehensible code. Fortunately, the PureScript language provides us everything we need to make asynchronous programming sound, comprehensible, type-safe, and easy! In this talk, John A. De Goes reviews the `purescript-aff` library and ecosystem, highlighting the power of the library, and showing that asynchronous programming can be just as easy &mdash; no, easier! &mdash; than synchronous programming. Only a basic knowledge of PureScript is assumed, so this talk can be considered a continuation of earlier presentations.

### Reactive UIs with Halogen

**@paf31**

**1 hour**

`purescript-halogen` is a new library for developing reactive front-end web applications in PureScript. I will give a brief overview of the ideas behind the library, and we will work through the creation of a simple application from scratch. Bring a laptop! 

### Introduction to Property Testing with QuickCheck

**@fresheyeball**

Making code reliable in JavaScript is a nightmare. Learn how property testing and PureScript can help provide better tests with greater coverage of the problem space. Walking through options available in JavaScript, translating Unit Tests to Property Tests. Closing with examples of Property Tests in PureScript, demonstrating how QuickCheck and PureScript can help you produce better code over Unit Tests and JavaScript.

### Making Sense of Project Build & Dependency Management

**@alex_berg**

**30 minutes**

_Abstract_

Things go smoothly when you do as the book instructs, but when you try to do something interesting, the compiler says, "Can't find module". Why can't it find my stuff? In this talk, we'll look at the recommended build tools, discuss what they do, and compare them to similar tools. Project dependency management will also be discussed, as build tools often need to know how it works.

### The Future of PureScript (Discussion)

**@paf31**

**1 hour**

Let's discuss the current state of PureScript, and the changes we want to see in the future. Come and tell us about the features you want to see at PureScript Conf 2016!

- New backends (C++11, Lua, Python, PHP, JVM, ...)
- Optimizations and rewrite rules
- PureScript's killer applications

### Call for Speakers

_Anyone_ who would like to present a topic related to PureScript development is more than welcome to do so. Any level of experience is appropriate, although we would like to encourage speakers to prepare something appropriate for a beginner-level audience. If you have a topic, please add the title and a brief description above.

## Attendees

- Phil Freeman (@paf31)
- John A. De Goes (@jdegoes)
- Greg Pfeil (@sellout)
- Brian McKenna (@puffnfresh)
- Isaac Shapira (@fresheyeball)
- Lane Seppala  (@laneseppala)
- Chris Wilson (@twopoint718)
- Jon Childress (@jonplussed)
- Grigoriy Belenkiy (@grishace)
- Daniel Santa Cruz (@dstcruz)
- Jeff Simpson (@fooblahblah)
- Jesse Frankley (@numberdotten)
- Graham Lipsman (@glipsman)
- Colt Frederickson (@coltfred)
- Joe Nash (@jna_sh)
- Ben Burdette
- Alan Shen (@sunzenshen)
- Alex Berg (@alex_berg)
- Ryan Tanner (@youfoundryan)
- Tyler Perkins (@thinksoutside)
- Gustavo Gomez
- Sean Garborg (@garborg)
- Peter J. Jones
- Michael Ficarra (@jspedant)
- Connor James
- Talon (@legittalon)
- Dave Rostron (@yastero)
- Garrett Dawson (@killtheliterate)
- Solomon White (@rubysolo)
- Mark Bolusmjak (@z5h)
- Mark Farrell (@markfarrell)
- Ana Chang (@anarchang)
- Emrys Ingersoll (@wemrysi)
- Don Abrams (@donabrams)
- Darin Morrison (@freebroccolo)
- Tyler Prete (@tprete)
- Todd Bernhard
- Jed Schneider (@jedschneider)
- Joe Douglas (@jjoedouglas)
- Eric Thomas (@et)
- Mark Nakasone
- Proctor (@stevenproctor)
- Erin Swenson-Healey (@lasericus)
- Beau Lyddon (@lyddonb)
- Matt Williamson (@bizarromatt)
- Matt Elmer (@mattelmer)
- Ryan Orendorff
- Robert Kluin (@robertkluin)
- John Coe (@4tPartySoftware)
- Leif Warner (@pdxleif)
- Kris Nuttycombe (@nuttycom)

and also on [Lanyrd](http://lanyrd.com/2015/purescript-conf/).