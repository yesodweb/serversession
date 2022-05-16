# serversession packages

Secure, modular server-side sessions.

This repo contains many packages that together implement
traditional server-side sessions.  Users who don't have a session
yet are assigned a random 144-bit session ID that is the key on a
storage backend.  All session data is saved on a storage
backend.  Includes 110+ unit and property tests.

The `serversession` package
([![Hackage](https://img.shields.io/hackage/v/serversession.svg)](https://hackage.haskell.org/package/serversession))
implements the core logic.  It needs to be paired up with two
companion packages:

  * _Backend (storage)_, in order to store the session data.
    Currently we support:

    * `serversession-backend-persistent`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-backend-persistent.svg)](https://hackage.haskell.org/package/serversession-backend-persistent)):
      Storage backend using `persistent`'s SQL backend.  Works
      with PostgreSQL, MySQL, SQLite.

    * `serversession-backend-acid-state`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-backend-acid-state.svg)](https://hackage.haskell.org/package/serversession-backend-acid-state)):
      Storage backend using `acid-state`.  This backend keeps
      sessions in memory but provides ACID guarantees using a
      transaction log.  It can also be used without durability as
      a memory-only backend.

    * `serversession-backend-redis`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-backend-redis.svg)](https://hackage.haskell.org/package/serversession-backend-redis)):
      Storage backend using Redis via the `hedis` package.

  * _Frontend_, bindings for your web framework of choice.
    Currently we support:

    * `serversession-frontend-yesod`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-frontend-yesod.svg)](https://hackage.haskell.org/package/serversession-frontend-yesod)):
      Support for the Yesod framework.  Replaces the default
      `clientsession`.

    * `serversession-frontend-snap`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-frontend-snap.svg)](https://hackage.haskell.org/package/serversession-frontend-snap)):
      Support for the Snap framework.  Replaces the default
      `Snap.Snaplet.Session.Backends.CookieSession` based on
      `clientsession`.

    * `serversession-frontend-wai`
      ([![Hackage](https://img.shields.io/hackage/v/serversession-frontend-wai.svg)](https://hackage.haskell.org/package/serversession-frontend-wai)):
      Generic support for WAI applications via the `wai-session`
      package by using a `vault`.

If your favorite storage backend or framework is not listed
above, please send us a pull request!  The `serversession`
package should work for any session that may be represented as a
mapping of keys to values.

Examples:

  * Using Yesod frontend + Persistent backend:
    [GitHub link](https://github.com/yesodweb/serversession/tree/master/examples/serversession-example-yesod-persistent/).


## Security notes

The session ID is generated via the `nonce` package, which in
turn uses a CPRNG created from AES on CTR mode.  The CPRNG is
reseed automatically from `/dev/urandom` (or equivalent)
periodically.  We use the base64url variant, thus providing 144
bits of entropy, which is more than enough to make guessing
session IDs impossible.

The session ID stays fixed most of the time.  Anonymous users
receive session IDs unless their session remains empty (as an
optimization).  The session ID can be invalidated in order to
prevent
[session fixation attacks](http://www.acrossecurity.com/papers/session_fixation.pdf),
either automatically (see below) or manually (via
`forceInvalidate`). On a session fixation attack, the attacker
convinces the victim to use the same session ID as his and asks
the victim to log in.  If the session is not invalidated upon
login, the attacker will now be in possession of a session ID
that is logged in as the victim.  If the session is invalidated,
the victim receives a new session ID that the attacker doesn't
have any knowledge of.

We support both idle timeouts and absolute timeouts.  Idle
timeouts invalidate the session if a given amount of time has
passed since the last request was made for a session.  Absolute
timeouts invalidate the session if a given amount of time has
passed since the session was created, no matter the activity.


## Authentication integration

We have special support for authentication plugins that save
information about the logged in user on a session variable:

  * The session key used by authentication plugin (e.g., `_ID`
    for `yesod-auth`) is recognized and saved separately on the
    database.  This allows you to quickly identify all sessions
    of a given user.  For example, you're able to implement a
    "log out everywhere" button.

  * Whenever the logged in user changes, the backend will also
    invalidate the current session ID and migrate the session
    data to a new ID.  This prevents session fixation attacks
    while still allowing you to maintain session state accross
    login/logout boundaries.

Any authentication mechanism is supported as long as it uses a
session variable.


## Storage optimizations

We provide the following storage optimizations:

  * Empty sessions are not saved.  This is done transparently:
    just insert a session variable and the session will
    materialize.  Note that if your framework always creates a
    CSRF token (e.g., Snap), then this optimization will not
    apply you.

  * You can set the timeout resolution.  Requests made within the
    timeout resolution that do not change any session variables
    will not update the session on the database.  By default the
    timeout resolution is set to 10 minutes.


## Session data types

The `serversession` family is generalized with respect to the
session data type.  We provide `SessionMap`, a `newtype` of `Map
Text ByteString`, that is guaranteed to work with every backend
and frontend.  However, you're able to create a custom data type
and use it as well, as long as you implement the typeclasses
needed by your backend and frontend.

The storage backends support basically anything they're able to
serialize.  The `persistent` backend accepts anything that can be
used as a `persistent` field.  The `acid-state` backend accepts
anything that can be serialized via `SafeCopy`.  The `redis`
backend accepts anything that can be stored as a Redis hash.

The frontends unfortunately are quite more limited.  Yesod
insists that sessions be `Map`s of `Text` to `ByteString`, while
Snap insists on `Map`s of `Text` to `Text`.  The `wai-session`
package allows any types of keys and values, but still expect a
map.  Until the frameworks get generalized as well, it's a bit
difficult to take advantage of custom session data types.


## Current limitations

These limitations may be addressed in the future.  Right now,
though, please bear in mind that:

  * There's no support for cleaning old sessions from the storage
    backends.

  * There's no way of setting timeouts and cookie persistence on
    a per-session basis, only on a global basis.

  * The Redis backend is not set to expire sessions yet.

  * The `persistent` backend does not automatically create an
    index for the auth ID.  Thus, by default the
    `deleteAllSessionsOfAuthId` operation will take linear time.

  * We stress test our backends to ensure they support almost
    every conceivable use case for sessions, but we only
    guarantee to support:

    * Every possible character and byte for keys and values,
      respectively.

    * At least one million keys per session.

    * Session values of at least 100 MiB.

    * Session keys of At least 1 MiB.

    * At least 200 independent sessions per user.


## Background

Yesod has always support client-side sessions via the
[`clientsession`](http://hackage.haskell.org/package/clientsession)
package: the session data is encrypted, signed, encoded and sent
to the client inside a cookie.  When receiving a request, the
cookie is decoded, verified and decrypted.  The server does not
have to maintain any state, so the client-side session backend is
as fast as the cryptographic primitives.

However, there are some disadvantages to client-side sessions:

  * _Replay attacks_.  It's not possible to invalidate a session,
    for example.  When logging out, a new cookie is sent with
    logged out session data.  However, as the server doesn't
    maintain state about sessions, it will still accept the old,
    logged in cookie until it expires.  One could set very small
    expiration times to mitigate this, but this would force users
    to relogin frequently.  This server-side backend allows you
    to maintain long expiration times while still having secure
    logouts.

  * _Cookie size_.  As the cookie contains the whole session data
    plus some overhead, care must be taken not to create too much
    session data.  Yesod already saves the logged in user ID via
    `yesod-auth` and a XSRF token via `yesod-form`.  This
    server-side backend uses a cookie of fixed size (24 bytes).

  * _No remote logout_.  In many instances it is desirable to
    invalidate sessions other than the current one.  For example,
    the user may have changed their password, or the the site
    provides a button to cancel all logged in sessions besides
    the current one.  This server-side backend allows you to
    invalidate sessions other than the current one via
    `forceInvalidate`.

  * _Missing key rotation_.  Ideally, `clientsession`'s keys
    should be rotated periodically.  In practice, support for key
    rotation has never been implemented on `clientsession`.  This
    server-side backend does not need to do key rotations, and
    the session ID CPRNG is automatically reseeded.

The `serversession` package is `clientsession`'s rival, each has
their own advantages and disadvantages.  However, both of them
can be used on different ecosystems and take security from the
ground up.


## Comparision to other packages

At the time of writing (2015-05-22), these are the session
packages that do not use either `clientsession` or
`serversession`:

  * `mysnapsession` (via `Memory` module, also supports
    `clientsession` mode): Server-side sessions.  Works for
    `snap`.  Weak session ID generation.  Vulnerable to session
    fixation attacks.  Cannot invalidate other sessions.

  * `salvia-sessions`: Server-side sessions.  Works only for
    `salvia`.  No built-in support for DB-backed sessions, only
    memory-backed ones.  Weak session ID generation.  Vulnerable
    to session fixation attacks.  Cannot invalidate other
    sessions.

  * `simple-session`: Client-side sessions.  Works for `simple`
    framework.  No encryption.  Authentication vulnerable to
    timing attacks.

  * `Spock` (formely `scotty-session`): Server-side sessions.
    Works for `Spock` (code is not packaged separately).  Only
    supports memory-backed sessions persisted on a file.  Weak
    session ID generation.  Vulnerable to session fixation
    attacks.  Cannot invalidate other sessions.

  * `wai-session`: Server-side sessions.  Works for `wai`
    applications.  Weak session ID generation.  Vulnerable to
    session fixation.  Cannot invalidate other sessions.
    Out-of-the-box support for TokyoCabinet only.

  * `yesod-session-redis`: Server-side sessions.  Works for
    Yesod and Redis.  Weak session ID generation via `random`.
    Vulnerable to session fixation.  Cannot invalidate other
    sessions.

We apologize in advance if any information above is incorrect.
Please contact us about any errors.


## Versioning

Assuming a version of `MAJOR.MINOR.PATCH`, the following
conventions are used by the `serversession` family of packages:

  * The PVP's versioning scheme is followed without changes.  The
    important part is that either `MAJOR` or `MINOR` needs to be
    increased whenever a breaking change is made, while only
    `PATCH` needs to be changed otherwise.

  * In addition to the PVP, we increment `MAJOR` only when the
    `serversession` core package makes a breaking change.  This
    means that:

    * The `serversession` core package always has `MINOR` equal
      to `0`.

    * The other `serversession-*` packages have the same `MAJOR`
      as `serversession`, while being free to have any `MINOR`.

The scheme above is used to ensure consistent versioning of all
packages even if they're not always released at the same time.
