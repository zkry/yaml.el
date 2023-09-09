# yaml.el

[![MELPA](https://melpa.org/packages/yaml-badge.svg)](https://melpa.org/#/yaml)

yaml.el is a YAML parser written in Emacs List without any external
dependencies.  It provides an interface similar to the Emacs JSON
parsing utility.  The functions provided are as follows:

``` emacs-lisp
(yaml-parse-string string &rest args)
```

The following keyword args are accepted:

- `:object-type` specifies the Lisp data structure to store parsed
  objects data in.  It takes the following symbols:
  - `hash-table` (default)
  - `alist`
  - `plist`
- `:object-key-type` specifies how map keys should be handled. It takes the following symbols:
  - `string`
  - `symbol` (default) Use symbols as keys.  If `:object-type` is `plist`, this becomes the same as `keyword`.
  - `keyword` Always use keywords as keys.
- `:sequence-type` specifies the Lisp data structure to store the
  parsed sequences in.  It takes the following symbols:
  - `array` (default)
  - `list`
- `:null-object` specifies the lisp object to use for nulls.  Defaults
  to the symbol `:null`.
- `:false-object` specifies the lisp object to use for false.
  Defaults to the symbol `:false`.

```emacs-lisp
(yaml-encode object)
```

The function `yaml-encode` will encode a Lisp object to a YAML string.


## Installation

Until this is published to MELPA you will need to use the code from this repo directly.
You can put yaml.el in you load path directly or use a tool like use-package or straight.el.


## Examples

``` emacs-lisp
(require 'yaml)

(yaml-parse-string "
recipe:
  ingredients:
  - milk
  - eggs
  - oil
  - flour
  duration: 10
  steps: null" :object-type 'alist
               :sequence-type 'array
               :null-object :empty)

;; => (("recipe" ("ingredients" . ["milk" "eggs" "oil" "flour"]) ("duration" . 10) ("steps" . :empty)))

(yaml-parse-string "
translations:
  one: бір
  two: екі
  three: үш")

;; => #s(hash-table ... data ("translations" #s(hash-table ...)))


(yaml-encode '("omitted" ((count . 3) (value . 10) (items ("ruby" "diamond"))) "omitted"))

;; => "
- omitted
- count: 3
  value: 10
  items:
    ruby: [diamond]
- omitted"


```

## Caveats

Since this is implemented in Emacs Lisp performance is probably not the best.  An alternative implementation using libyaml exists and can be found [here](https://github.com/syohex/emacs-libyaml).

If you have a very deeply nested YAML file and your `max-lisp-eval-depth` variable is set too low, these is a chance that you might hit the maximum Lisp eval depth limit.  In the future I may work on changing the parsing algorithm to avoid this problem but in the meantime you can bump up the `max-lisp-eval-depth` variable in your config.

## Development

You can run the tests by executing

```
$ ~/path/to/git-repo/yaml.el
$ emacs -batch -l ert -l yaml.el -l yaml-tests.el -f ert-run-tests-batch-and-exit
```
