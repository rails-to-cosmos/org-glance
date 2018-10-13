Org-glance: walk in the woods with pleasure
===============================

Fast, clean and convenient way of traversing your org-mode forest.

Turn org-mode into context-aware bookmark storage, password manager,
flexible build system or whatever you can imagine with ease.

## Running tests

### Batch mode

    emacs -batch -l ert -l org-glance-tests.el -f ert-run-tests-batch-and-exit

### Org-mode

You can build project using `C-x y m` key or running `org-glance-devtools/build` command from emacs. You need to evaluate Devtools blocks in org-glance.org to do so. Devtools initialize when you open org-glance.org by default.
