## About This Project
This project enjoys long walks on the beach, fluffy dogs and staring deeply
into the glowing light of a command line terminal.

But seriously...
Contained within this project is a program meant to run on a server for competing
in an AI Bot Challenge run by [The AI Games](http://theaigames.com). Specifcally,
[Ultimate Tic Tac Toe](http://theaigames.com/competitions/ultimate-tic-tac-toe/rules)

Is our bot any good? Probably not, but it's ours, and we love it anyways.

## Running

### In the terminal
Although it won't be very exciting, in the terminal, assuming leiningen is installed,
navigate to the root of the project folder and enter:

    => lein run

### And if I don't have Leiningen or Clojure installed?
Leiningen has tools for packaging the program as a JAR file. We don't have one yet,
but eventually we will.

## Testing with [Midje](https://github.com/marick/Midje)

### Setup
1) In ~/.lein/profiles.clj and add:

    {:user {:plugins [[lein-midje "3.1.3"]]}}

### [Tutorial](https://github.com/marick/Midje/wiki/A-tutorial-introduction)

### Running your tests

#### Autotest
Autotest will watch your files and rerun your tests any time you save your files.
To run Autotest from an active repl:

    => (use 'midje.repl)
    => (autotest)

#### Run tests once only
If you just want to run your tests once, from the terminal navigate to the root of
your project and enter:

    => lein midje
