Simple REST / SMTP gate
=======================

Requirements
------------

1. Install Erlang/OTP (http://www.erlang.org/).

  $ sudo apt-get install erlang

2. Download and build Rebar tool (http://github.com/basho/rebar/):

  $ git clone https://github.com/basho/rebar.git

  $ cd rebar

  $ ./bootstrap

Build
-----

1. Copy `rebar` executable file into this (restmail/) directory.

2. Create config file:

  $ make config

3. Edit SMTP options in config file "include/restmail.hrl".

4. Build application:

  $ make

Run
---

  $ ./restmail {start|stop|attach|console}

Usage
------

Demo using Python shell:

  >>> import httplib2, json
  >>> h = httplib2.Http()

  >>> msg = json.dumps({"recipients": ["test1@example.com", "test2@example.com"],
                        "subject": "Welcome",
			"body": "<h1>The body of this message</h2>"})

  >>> r,c = h.request("http://localhost:9876/mail",
                       method="POST",
		       body=msg,
		       headers={"Content-Type":"application/json"})

IP and port
-----------

* Default listening port is 9876, configurable in include/restmail.hrl.
* Default server IP is 127.0.0.1, configurable via RESTMAIL_IP environment variable:

  $ export RESTMAIL_IP=0.0.0.0 # then restart me.
