type-level-json
===========================

[RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259) compliant parser for JSON, within the Haskell type system.

## Table of Contents
- [Example](#example)
- [Result](#result)
- [Thoughts](#thoughts)
- [License](#license)

### Example

```haskell
λ> :kind! Decode "{ \"a\" : null, \"hey\" : \"lol\", \"sub\" : { \"subObj\" : [\"foo\",true], \"key\" : false }}" :: Value
```

### Result

```haskell
= 'Object
	'[ '("a", 'Null), '("hey", 'JString "lol"),
	   '("sub",
		 'Object
		   '[ '("subObj", 'Array '[ 'JString "foo", 'Bool 'True]),
			  '("key", 'Bool 'False)])]
```

### Thoughts

Still experimental, could potentially be used for reifying parsers, encoders and extended to support JSON schema. Could also validate `jq` / `jsonpath` queries against the defined schema.

## License

[BSD3](LICENSE) 2022-2023 (c) David Johnson.
