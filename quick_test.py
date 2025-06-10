import sanma_engine
import numpy as np

# This test now correctly uses the legal_actions mask to play a valid move.
env = sanma_engine.Env()
obs, legal_actions = env.reset()
print("obs shape:", obs.shape)
print(f"Initial legal actions: {np.sum(legal_actions)} options")

for i in range(5): # Let's run a few more steps
    # Find the first available legal action from the mask
    legal_action_indices = np.where(legal_actions)[0]
    assert len(legal_action_indices) > 0, "No legal actions available!"
    action_to_take = legal_action_indices[0]
    
    print(f"\nTurn {i+1}: Taking action {action_to_take}")
    obs, reward, done, info = env.step(action_to_take)
    
    # Update the legal_actions mask for the next turn
    legal_actions = info['legal_actions_mask']
    
    print(f"Reward: {reward}, Done: {done}")
    if done:
        print("Round finished!")
        break