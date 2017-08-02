# markco

Collaborative Markdown authoring (with scientific addons).

**pre-alpha**

## Build/Install

Docker is recommended, `Dockerfile` is in `/dist`.

In project root:

```bash
make
docker build --tag markco dist
docker run -it --rm markco
```

## Development

To run a dev-version locally,
install [`caddy`](https://caddyserver.com/), then:

```bash
cd server/
mkdir data/
stack build
caddy
```

## Userfile format

See `server/users.txt` for an example. It contains a record for username `test` with password `markco`.

Format is:

```bash
${username}:sha1("${username}:${password}")
```

You can generate the second part (`sha1sum("${username}:${password}"` that is) with a script looking something like this:

```bash
echo -n test:markco | sha1sum
```
