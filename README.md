# elm-rails-routes

This program generates small path helper functions for Elm from the output of `rails routes`.


## Usage

First run the following command in your Rails root:

```
$ rails routes > routes
```

Then run

```
$ elm-rails-routes init
$ elm-rails-routes generate-elm
```

This will create an Elm module under `src/Routes.elm` which content looks
something like this (depending on your actual rails routes):

```elm
module Routes exposing (..)

import Http


users : String
users =
    [ "users"
    ]
        |> String.join "/"
        |> String.append "/"

user : String -> String
user id =
    [ "users"
    , Http.encodeUri id
    ]
        |> String.join "/"
        |> String.append "/"

...
```


## Configuration

Take a loot at `elm-rails-routes.json` to adjust the configuration.
