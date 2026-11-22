# Superchat for other packages

How a package, a plugin or a skill uses Superchat. For the full contract, see [`SPEC.md`](../SPEC.md).

## `superchat-run`

One function gives any command an AI layer:

```elisp
(superchat-run (superchat-tape-new-name) "Summarise this: ..."
               :tools 'none :ephemeral t
               :on-done (lambda (run)
                          (pcase (plist-get run :status)
                            ('done (message "%s" (plist-get run :text)))
                            ('error (message "%s" (plist-get run :error))))))
```

Leave `:tools` out to give the model its tools; `:unattended t` makes a run that waits for an answer instead of asking.

## Plugins

A plugin adds tools, views and chat commands:

```elisp
(superchat-plugin-define 'weather
  :doc "Tell the model the weather."
  :enable (lambda ()
            (superchat-tool-define
             "weather" :class 'read :description "Weather of a city."
             :args '((:name "city" :type string :required t))
             :function (lambda (args) (my-weather (plist-get args :city)))))
  :disable (lambda () (superchat-tool-remove "weather")))
(setq superchat-plugins '(weather))
```

## Typed tools in a skill

A skill can also declare typed tools in an `interface.eld` beside its `SKILL.md`, each a piece of Lisp with typed arguments and a class; `/learn` writes these for you; [learn.md](learn.md) shows a skill growing over three rounds. See `SPEC.md`.
