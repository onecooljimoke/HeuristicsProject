## Testing with [Midje](https://github.com/marick/Midje)
### Setup
1) In ~/.lein/profiles.clj and add:

    {:user {:plugins [[lein-midje "3.1.3"]]}}

### [Tutorial](https://github.com/marick/Midje/wiki/A-tutorial-introduction)

### Autotest
Autotest will watch your files and rerun your tests any time you save your files.
To run Autotest from an active repl:

    => (use 'midje.repl)
    => (autotest)
