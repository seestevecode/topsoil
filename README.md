# Topsoil

Topsoil is an Elm port of a game I used to play on my iPhone.

## Status

The project is currently parked in a clean working state, while future improvements are planned.

## Running locally

Build the app:

```bash
elm make src/Main.elm --output=app.js
```

Serve the project locally, for example:

```bash
python3 -m http.server 8000
```

Then open: http://localhost:8000

## Roadmap

These are in no particular order and are intended only as an _aide memoire_ for me. There is no commitment that all (or indeed any) of these will be implemented.

- [ ] Dark mode
- [ ] Help bar
- [ ] High scores (global and local)
- [ ] Make replayable from seed
- [ ] Responsive layout
- [ ] Review and document scoring system
- [ ] Start new game
- [ ] Tutorial
- [ ] User-selected themes

## Deployment

GitHub Pages is deployed from the `main` branch using GitHub Actions: https://code.seesteve.xyz/topsoil.
