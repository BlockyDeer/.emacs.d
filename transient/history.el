((magit-branch nil)
 (magit-commit
  ("--verbose")
  nil)
 (magit-diff
  ("--stat" "--no-ext-diff")
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-gitignore nil)
 (magit-log
  ("--decorate" "--graph" "-n256")
  ("--decorate" "--graph")
  ("-n256" "--graph" "--decorate")
  ("-n256" "--graph" "--color" "--decorate"))
 (magit-merge
  ("--ff-only")
  nil)
 (magit-push
  ("--force")
  nil)
 (magit-rebase
  ("--autostash")
  nil
  ("--autostash" "--interactive"))
 (magit-remote
  ("-f"))
 (magit-reset nil)
 (magit-stash nil
              ("--include-untracked"))
 (magit-submodule nil)
 (magit-tag nil))
