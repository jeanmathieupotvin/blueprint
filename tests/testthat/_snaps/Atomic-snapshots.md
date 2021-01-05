# $print() output when length > 0L

    <Atomic blueprint [0.0.0.9002]>
      <name:test type:integer length:10>

# $print() output when length is NULL

    <Atomic blueprint [0.0.0.9002]>
      <name:test type:integer>

# $as_yaml() output with headers; using default handlers

    source: R[v3.6.3]::blueprint[v0.0.0.9002]::Atomic$as_yaml()
    UTF-8 char: é
    Atomic:
      name: test
      type: raw
      length: 5
      prototype: AA==

# $as_yaml() output without headers; using default handlers

    Atomic:
      name: test
      type: raw
      length: 5
      prototype: AA==

# $as_yaml() output with custom parameters; using custom handlers

    source: R[v3.6.3]::blueprint[v0.0.0.9002]::Atomic$as_yaml()
    Atomic:
        name: test
        type: raw
        length: 5
        prototype: '00'

# $as_json() output with headers

    {
      "source": "R[v3.6.3]::blueprint[v0.0.0.9002]::Atomic$as_json()",
      "UTF-8 char": "é",
      "Atomic": {
        "name": "test",
        "type": "raw",
        "length": 5,
        "prototype": "AA=="
      }
    }

# $as_json() output without headers

    {
      "Atomic": {
        "name": "test",
        "type": "raw",
        "length": 5,
        "prototype": "AA=="
      }
    }

# $as_json() output with custom parameters

    {
      "source": "R[v3.6.3]::blueprint[v0.0.0.9002]::Atomic$as_json()",
      "Atomic": {
        "name": "test",
        "type": "raw",
        "length": 5,
        "prototype": 0
      }
    }

