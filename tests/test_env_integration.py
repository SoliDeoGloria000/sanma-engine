import pytest
import numpy as np

# This import will work after you run `maturin develop`
from sanma_engine import Env 

def test_env_creation_and_reset():
    """Tests if the environment can be initialized and reset."""
    env = Env(seed=123)
    assert env is not None, "Environment object could not be created."
    obs, legal_actions = env.reset(seed=456)
    assert obs.shape == (149, 5, 5), f"Observation shape is wrong: {obs.shape}"
    assert legal_actions.shape[0] > 100, "Legal actions array seems too small."
    print("\n✅ Environment created and reset successfully.")

def test_initial_observation_and_legal_actions():
    """
    Tests if the initial observation and legal actions match what we expect
    for a fixed seed.
    """
    env = Env(seed=1)
    obs, legal_actions = env.reset(seed=1)
    num_legal_actions = np.sum(legal_actions)
    assert num_legal_actions > 0, f"Expected at least one legal action, found {num_legal_actions}."
    print(f"\n✅ Found {num_legal_actions} initial legal actions.")

    # Check the Kita count plane. This is at index 17 (12+4+1).
    kita_count_plane_idx = 12 + 4 + 1 
    kita_count = obs[kita_count_plane_idx, 0, 0]
    assert kita_count == 0, f"Initial Kita count should be 0, but got {kita_count}."
    print("✅ Initial Kita count is correctly set to 0.")


def test_basic_step():
    """Tests if the environment can take a basic step without crashing."""
    env = Env(seed=777)
    obs, legal_actions = env.reset()
    action_id = np.where(legal_actions)[0][0]
    assert action_id != -1, "No legal action found in the initial state."
    try:
        next_obs, reward, done, info = env.step(action_id)
        assert isinstance(next_obs, np.ndarray)
        assert isinstance(reward, float)
        assert isinstance(done, bool)
        assert isinstance(info, dict)
        print(f"\n✅ Step successful with action {action_id}.")
    except Exception as e:
        pytest.fail(f"Env.step() failed with action_id {action_id}: {e}")
        
def test_observation_planes_on_reset():
    """
    Tests if the newly added observation planes (winds, scores) are correct
    on a clean reset.
    """
    env = Env(seed=1)
    obs, _ = env.reset(seed=1)

    # Player 0 is current. Round should be East.
    # Index is now 18 (12+4+1+1).
    round_wind_plane_idx = 12 + 4 + 1 + 1 
    round_wind_val = obs[round_wind_plane_idx, 0, 0]
    assert round_wind_val == 27, f"Expected Round Wind to be East (27), got {round_wind_val}"

    # Scores plane. P0 score is 35000. We store it as 35 (35000 / 1000).
    # New Index: 19(round wind)+3(seat winds) = 22
    scores_plane_idx = round_wind_plane_idx + 1 + 3
    p0_score_val = obs[scores_plane_idx, 0, 0]
    assert p0_score_val == 35, f"Expected P0 score to be 35, got {p0_score_val}"
    
    riichi_sticks = obs[scores_plane_idx, 1, 0]
    honba_sticks = obs[scores_plane_idx, 1, 1]
    assert riichi_sticks == 0, "Initial riichi sticks should be 0"
    assert honba_sticks == 0, "Initial honba sticks should be 0"

    print("\n✅ Newly added observation planes (winds, scores) verified on reset.")