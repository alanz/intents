# intents

Very simple start of an offers and needs app for Holochain.

Copy-pasted from holoworld.

## To install and run:

* [Install Holochain](https://developer.holochain.org/Install_Holochain) if you don't have it installed already.
* Clone this repo to your dev computer.
* In the intents directory, do:
```
hcdev web
```
* Open a browser to http://localhost:4141

Roadmap:

* Add structure to intents: type (offer, need, etc), category, optional resource classification, quantity and unit, description, some dates).
* Add searches.
* Add methods of matching of offers and needs.
* Add [Conversation for Action](https://www.valueflo.ws/introduction/cfa.html) = coming to agreements on exchanges around offers and needs.

## Building the purescript stuff

This uses [halogen](https://github.com/slamdata/purescript-halogen/tree/master/docs).

In the PureScript ecosystem [Bower](http://bower.io/) is currently the
most commonly used package manager and we'll be relying on it for this
project, so if you don't already have it, you can install it like
this:

``` shell
npm install --global bower
```

If you don't already have a global installation of the PureScript
compiler and [Pulp](https://github.com/bodil/pulp) (or you want a
local installation with the appropriate versions) you can run:

``` shell
npm install
```

Finally you'll need to install the PureScript library dependencies for
this project with Bower:

``` shell
bower install
```

## Building

The project can now be built with:

``` shell
npm run build
```

This will build the PureScript source code and produce a bundled JS
file as `dist/app.js`.

This is an alias for the Pulp command:

``` shell
pulp build --to dist/app.js
```

If you open `dist/index.html` you should now have a basic working Halogen app.

You can also use the command:

``` shell
npm run watch
```

To start a process that will watch the source files and trigger a
reload whenever they are modified. Alternatively...

## Fast watching with `purs ide`

If you're using an editor that supports `purs ide` or running
[`pscid`](https://github.com/kRITZCREEK/pscid) there's an option for
getting near-instant builds of the app while you work on it:

``` shell
npm run watch-fast
```

This will start a watch process that uses
[Webpack](https://github.com/webpack/webpack) to rebundle the app
whenever the _output_ files are changed. Since `purs ide` rebuilds
modules on save, this means you can use this much faster bundle-only
rebuild script.

:warning: `purs ide` only rebuilds one module at a time, so sometimes
the bundle will end up in an inconsistent state, resulting in runtime
errors. This occurs when a change is made in one module that breaks
other modules that depend on it. The solution is to run a full build
when a change like this is made, as the compiler will force you to
resolve those errors.


---------------------------------

Captured log (wireshark)

```
POST /fn/Intents/intentCreate HTTP/1.1
Host: localhost:4141
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:59.0) Gecko/20100101 Firefox/59.0
Accept: */*
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Referer: http://localhost:4141/
Content-type: application/json
Content-Length: 88
DNT: 1
Connection: keep-alive

{"content":"This text will be saved in Holochain","type":"Offer","timestamp":1525534245}

HTTP/1.1 200 OK
Date: Sat, 05 May 2018 15:30:45 GMT
Content-Length: 48
Content-Type: text/plain; charset=utf-8

"QmTFUsW69e9WcA2SfYaLC8eKx65uHrwEvotyn5nqzHmTWj"
```

Reading an intent
```
POST /fn/Intents/intentRead HTTP/1.1
Host: localhost:4141
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:59.0) Gecko/20100101 Firefox/59.0
Accept: */*
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Referer: http://localhost:4141/
Content-type: application/json
Content-Length: 48
DNT: 1
Connection: keep-alive

"QmR7f4ftRFpxFa7Ye8KGuEMoYQDuCHYshJS8wVaKdyTnGp"

HTTP/1.1 200 OK
Date: Sat, 05 May 2018 18:50:41 GMT
Content-Length: 104
Content-Type: text/plain; charset=utf-8

{"content":"This text will be saved in Holochain","timestamp":"2018-05-05T18:50:40.260Z","type":"Offer"}
```

Getting all intents
```
POST /fn/Intents/getAllIntents HTTP/1.1
Host: localhost:4141
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:59.0) Gecko/20100101 Firefox/59.0
Accept: application/json
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Referer: http://localhost:4141/
Content-Length: 0
Content-Type: text/plain;charset=UTF-8
DNT: 1
Connection: keep-alive

HTTP/1.1 200 OK
Date: Sun, 06 May 2018 18:15:35 GMT
Content-Length: 501
Content-Type: text/plain; charset=utf-8

[{"Entry":{"content":"This text will be saved in Holochain","timestamp":"2018-05-06T18:15:33.125Z","type":"Offer"},"EntryType":"intent","Hash":"QmbtZRp6ibckQLJDGcp4umwL6K4iAPbbbi4n6QEBk37h8L","Source":"QmS85jqpzwsUaAnwjowbhkXvA9EfZAD6L3ToP85qw8S2BG"},{"Entry":{"content":"This text will be saved in Holochain","timestamp":"2018-05-06T18:14:37.733Z","type":"Offer"},"EntryType":"intent","Hash":"QmRLLxS2EReqCZyjem6AgDnEDrPYQeroywubMBzCNBQYHQ","Source":"QmS85jqpzwsUaAnwjowbhkXvA9EfZAD6L3ToP85qw8S2BG"}]
```
