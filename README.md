# sanma-engine

## Setup

Install Python dependencies and build the Rust extension:

```bash
pip install -r requirements.txt
maturin develop --release
```

Run tests with:

```bash
pytest -q
```

Prepare dataset files with:

```bash
python prepare_dataset.py
```
