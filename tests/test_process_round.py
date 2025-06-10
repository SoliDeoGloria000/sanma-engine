import json
import os
import sys
import pytest

sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from prepare_dataset import process_round, Env


def test_process_round_sample():
    with open('tests/data/sample_round.json', 'r') as f:
        data = json.load(f)
    round_data = data['log'][0]
    env = Env()
    pairs = process_round(round_data, env)
    assert len(pairs) == 33
