pkgx - Easy packaging of Erlang releases 
========================================

This program can be used to make OS packages for an Erlang application
bundled as an OTP release.

Getting started
---------------

Create a ``pkgx.config`` in the root of your project. Look at the
``pkgx.config.sample`` file for inspiration. Also, make sure that
you're all set with generating a release using ``rebar3``.

Typical workflow scenario:

    $> rebar3 release
    $> pkgx deb

Now, you have a `*.deb` file in your ``_build/prod/rel/`` directory
which holds the Erlang release and can be easily installed on a target
system.

The service will automatically start (using an ``init.d`` script), and
run under a new user account. Log files are found in
`/var/log/(package)`. A default, empty, config file is created in
`/etc/(package)/(package).config`, from which application environment
variables are read from.

