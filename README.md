# reference

https://github.com/juxt/aero

https://github.com/edn-format/edn

# run

``` zsh
$ clj -M:run
```

in clojure repl..

``` clojure
aero.core => (pprint (read-config "config.edn" {:transforms [:path]}))
```
