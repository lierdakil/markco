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

```bash
cd server/
mkdir data/
stack build
env MARKCO_USER_FILE=users.txt MARKCO_NEWCOMMANDS=../client/html/newcommands.tex stack exec markco-exe
```

It will run on port 8081 by default.

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

## Environment variables

See `server/src/Config.hs` and `dist/Dockerfile` for available options. You will want to set, at least, `MARKCO_USER_FILE`, which should point to a user-file.
