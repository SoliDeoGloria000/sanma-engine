# sanma-engine

## Setup

Follow these steps to build and verify the project:

1. **Install dependencies and `maturin`:**
   ```bash
   pip install -r requirements.txt
   pip install maturin
   ```

2. **Build and install the Rust extension:**
   ```bash
   maturin develop --release
   ```

3. **Run the tests to verify the installation:**
   ```bash
   pytest -q
   ```

`prepare_dataset.py` also relies on the compiled extension, so make sure it is built before running:

```bash
python prepare_dataset.py [--keep-pass]
```

Use the `--keep-pass` flag if you want to include PASS actions in the generated dataset.
