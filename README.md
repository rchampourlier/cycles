# Cycles

Product Cycles management tool for JobTeaser Tech Team

## Status

**WORK IN PROGRESS!!!**

Not usable at this point.

## Prerequisites

- Elm

### Install

```
git clone https://github.com/rchampourlier/cycles cycles
cd cycles
```

Install all dependencies using the handy `reinstall` script:

```
npm run reinstall
```

*This does a clean (re)install of all npm and elm packages, plus a global elm install.*

Reinstall elm dependencies:

```
npm run elm-install
```

*This removes `elm-stuff` and run `elm-install`.*


### Serve locally

```
npm start
```

* Access app at `http://localhost:8080/`
* Get coding! The entry point file is `src/elm/Main.elm`
* Browser will refresh automatically on any file changes..


### Build & bundle for prod

```
npm run build
```

* Files are saved into the `/dist` folder
* To check it, open `dist/index.html`


### References

- Initialized using https://github.com/elm-community/elm-webpack-starter
