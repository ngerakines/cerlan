# CalendERL About Nothing

An Erlang port of the beloved Calender About Nothing.

# Getting Started

First, be sure to create the file 'cerlan_github.hrl' in the src directory. It is currently being git-ignored. Inside that file, define LOGIN and TOKEN. These are used to interact with the GitHub API.

From there, you just need to build and run the system.

    $ make
    $ make dev &
    $ make dev-server &
    $ open http://127.0.0.1:8084

# Credits

This project is a direct result of using and loving Calendar About Nothing.

# License

Copyright (c) 2009 Nick Gerakines <nick@gerakines.net>

This project is open source under the terms of the MIT license.
