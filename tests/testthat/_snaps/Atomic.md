# $print()

    <Atomic blueprint [0.0.0.9001]>
      <name:test type:integer length:10>

# $as_yaml() output with default handlers

    source: R[v3.6.3]::blueprint[v0.0.0.9001]::Atomic$as_yaml()
    UTF-8 char: é
    Atomic:
      name: test
      type: raw
      length: 5
      prototype: AA==

# $as_yaml() output with custom handlers

    source: R[v3.6.3]::blueprint[v0.0.0.9001]::Atomic$as_yaml()
    UTF-8 char: é
    Atomic:
        name: test
        type: raw
        length: 5
        prototype: '00'

# $as_json() output

    {
      "source": "R[v3.6.3]::blueprint[v0.0.0.9001]::Atomic$as_json()",
      "UTF-8 char": "é",
      "Atomic": {
        "name": "test",
        "type": "raw",
        "length": 5,
        "prototype": 0
      }
    }

