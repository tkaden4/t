# t

A TFTP server written in haskell.

The goal is to support the operations defined in 
- [ ]  [RFC 1350](https://tools.ietf.org/html/rfc1350) (Basic Features),
- [ ]  [RFC 2347](https://tools.ietf.org/html/rfc2347) (Options),
- [ ]  [RFC 2348](https://tools.ietf.org/html/rfc2348) (Block size),
- [ ]  [RFC 2349](https://tools.ietf.org/html/rfc2349) (Transfer Size + Timeout)

## Usage
```Bash
t [--root-dir <dir>] [--port <port>]
```

## TODO (Other features)
- [ ] Daemonization
- [ ] Packaging and Distribution
