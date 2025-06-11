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


def test_process_round_continues_on_error(monkeypatch):
    with open('tests/data/sample_round.json', 'r') as f:
        data = json.load(f)
    round_data = data['log'][0]

    baseline_env = Env()
    baseline_pairs = process_round(round_data, baseline_env)

    class WrapperEnv:
        def __init__(self):
            self.inner = Env()
            self.count = 0

        def reset(self, *a, **kw):
            return self.inner.reset(*a, **kw)

        def get_game_phase_pystr(self):
            return self.inner.get_game_phase_pystr()

        def current_player_idx_py(self):
            return self.inner.current_player_idx_py()

        def get_obs_and_legal_actions(self):
            return self.inner.get_obs_and_legal_actions()

        def step(self, action_id):
            result = self.inner.step(action_id)
            self.count += 1
            if self.count == 5:
                raise ValueError('boom')
            return result

        def get_last_drawn_tile_for_current_player_val(self):
            return self.inner.get_last_drawn_tile_for_current_player_val()

    env = WrapperEnv()

    pairs = process_round(round_data, env)

    assert len(pairs) == len(baseline_pairs)


def test_process_round_handles_zero_tile():
    with open('tests/data/zero_tile_round.json', 'r') as f:
        data = json.load(f)
    round_data = data['log'][0]
    env = Env()
    pairs = process_round(round_data, env)

    # Should parse same number of actions as the normal sample round
    with open('tests/data/sample_round.json', 'r') as f:
        base = json.load(f)
    expected_pairs = len(process_round(base['log'][0], Env()))

    assert len(pairs) == expected_pairs
