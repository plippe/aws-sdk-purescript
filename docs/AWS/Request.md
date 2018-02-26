## Module AWS.Request

#### `RequestError`

``` purescript
data RequestError :: Effect
```

#### `request`

``` purescript
request :: forall eff i o. String -> String -> i -> Aff (err :: RequestError | eff) o
```


