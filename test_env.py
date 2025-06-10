import sanma_engine
import numpy as np

env = sanma_engine.Env(42)
obs, legal_actions = env.reset(seed=42)
print("initial hand0 size:", env.get_hand_size(0))

# Find the first legal discard action
action_to_take = -1
for i, is_legal in enumerate(legal_actions):
    # Action IDs 0-33 are discards
    if is_legal and 0 <= i <= 33:
        action_to_take = i
        break

assert action_to_take != -1, "Could not find a legal discard action"

print(f"Attempting to discard tile with action ID: {action_to_take}")
obs, r, done, info = env.step(action_to_take)
print("after discarding, hand0 size:", env.get_hand_size(0))