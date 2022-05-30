I had no choice but to delete `serverSessionDefs` to match persistent updates.
If you are using it as Example, please use `serverSessionDefsBySessionMap`.
If you are customizing in detail, please use `mkServerSessionDefs` as hard as you can.

# 1.0.5

* bump persistent to 2.10

# 1.0.4

* Default auth id to NULL to fix a MySQL bug.

# 1.0.3

* Get building on ghc-8
* Limit column size for session key
