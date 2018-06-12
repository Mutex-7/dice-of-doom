# dice-of-doom

A clojure port of the game "dice of doom" from the book "Land of Lisp".
I wrote this as a way of teaching myself some clojure.
There are still many ways in which I still need to improve this code, and I'll get around to that once I set up a blog or something.

## Prerequisites

You will need [Leiningen][] 2.0.0 or above installed.

[leiningen]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

    lein ring server

Or in the case where you don't want to install leiningen, just use the JAR file I've built:

	java -jar dice.jar <port-number>

This will start a webserver on localhost:5000 if no port number is provided.
