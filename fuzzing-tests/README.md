# Fuzzing tests for Aqua Compiler

## Installation

```sh
python3 -m pip install -r requirements.txt
```

## Usage

File `Aqua.g4` contains ANTLRv4 grammar of Aqua Language.

The following command will generate python fuzzing input generators for Aqua in `generated` dir:
```sh
grammarinator-process Aqua.g4 -o generated
```

The following command will generate `N` tests in `tests` dir with maximum grammar depth `D`:
```sh
grammarinator-generate -p generated/AquaUnparser.py -l generated/AquaUnlexer -r prog -n N -d D
```