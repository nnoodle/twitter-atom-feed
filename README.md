# twitter-atom-feed
Turns Twitter timelines into
[Atom](https://en.wikipedia.org/wiki/Atom_(standard)) feeds.

## Requirements
- Quicklisp
- Twitter API keys

## Usage
Compile with `make`, run `./twitter-atom-feed`, go to any of the
routes for tweets, e.g. http://localhost:8080/home?filters=image-p

Run `./twitter-atom-feed --help` for more options.

### Routes
- `/home`, home feed
- `/user/id/:id`, user's feed by id
- `/user/name/:name`, user's feed by name

### Parameters
- `count`, number of tweets—defaults to 200
- `filters`, comma separated list of functions from
  `twitter-atom-feed-filters` package. Functions starting with `-` will
  be negated, e.g. `-retweet-p` will remove non-retweets. See
  `filters.lisp` for the list of built-in filters.

### `config.lisp`
Located in `$XDG_CONFIG_HOME/twitter-atom-feed/config.lisp`, you can
define your own filters here. Make sure it begins with `(in-package #:twitter-atom-feed-filters)`

## License
Licensed under [ISC](./LICENSE)
