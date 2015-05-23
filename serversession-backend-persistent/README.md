# serversession-backend-persistent

This is the storage backend for `serversession` using
`persistent` and an RDBMS.  Please
[read the main README file](https://github.com/yesodweb/serversession/blob/master/README.md)
for general information about the serversession packages.

Unfortunately it is not easy to support all `persistent` backends
on a single package, and this is why we currently support the SQL
backend only (which is more commonly used).
