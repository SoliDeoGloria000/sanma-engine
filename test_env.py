import sanma_engine
import numpy as np


def main():
    env = sanma_engine.Env()
    obs, legal_actions = env.reset(seed=42)
    print("initial hand0 size:", env.get_hand_size(0))

    action_to_take = -1
    for i, is_legal in enumerate(legal_actions):
        if is_legal and 0 <= i <= 33:
            action_to_take = i
            break

    assert action_to_take != -1, "Could not find a legal discard action"

    print(f"Attempting to discard tile with action ID: {action_to_take}")
    obs, r, done, info = env.step(action_to_take)
    print("after discarding, hand0 size:", env.get_hand_size(0))


if __name__ == "__main__":
    main()
