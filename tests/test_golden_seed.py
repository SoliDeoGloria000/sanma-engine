import pytest
import numpy as np
from sanma_engine import Env

def test_reproducible_short_game():
    """
    Plays a fixed sequence of moves with a "golden" seed and asserts
    the final scores are always the same, ensuring game determinism.
    """
    GOLDEN_SEED = 1337
    ACTION_SEQUENCE_LENGTH = 5
    EXPECTED_FINAL_SCORES = [35000, 35000, 35000]

    env = Env()
    obs, legal_actions = env.reset(seed=GOLDEN_SEED)

    for i in range(ACTION_SEQUENCE_LENGTH):
        # Find the first available legal discard action
        action_id = np.where(legal_actions & (np.arange(len(legal_actions)) <= 33))[0][0]
        
        obs, _, done, info = env.step(action_id)
        
        assert not done, f"Game ended prematurely on turn {i+1}"
        legal_actions = info['legal_actions_mask']

    final_scores = [info[f'player_{i}_score'] for i in range(3)]
    
    assert final_scores == EXPECTED_FINAL_SCORES, \
        f"Golden seed test failed! Got {final_scores}, expected {EXPECTED_FINAL_SCORES}"

    print("\n✅ Golden seed regression test passed.")
