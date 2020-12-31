# twitter-atom-feed
Turns Twitter timelines into
[Atom](https://en.wikipedia.org/wiki/Atom_(standard)) feeds.

## Requirements
- Quicklisp
- Twitter API keys

## Usage
Compile with `make`, run `./twitter-atom-feed`, go to any of the
routes for tweets, e.g. https://localhost:8080/home

Run `./twitter-atom-feed --help` for more options.

### Routes
- `/home`, home feed
- `/user/id/:id`, user's feed by id
- `/user/name/:name`, user's feed by name

### Queries
- `imagep`, only tweets with images
- `count`, number of tweets—defaults to 200

## License
Licensed under [ISC](./LICENSE)
