# staat

staat is service summary page (like
[Github Status](https://status.github.com/)] with
[Zabbix](http://zabbix.com) backend.

The main intent of this application is to give public access to your
server and service statuses without exposing their details by
providing direct Zabbix access. In short, you want a short and simple
summary for a quick view.

## Usage

The best way is to compile the whole application as binary and run
it. Make sure to create 'staat.conf' (you can use 'staat.conf.example'
as starting point) and put it in the same folder where the binary is.

To customize the look, you can edit 'template.lisp' file.

## Installation

To compile it, use [Quicklisp](http://quicklisp.org). Running:

```
(ql:quickload :staat)
```

will download all dependencies and compile the application. Tu run it
from REPL, execute:

```
(staat:main)
```

## License

BSD.
