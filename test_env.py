import sanma_engine

env = sanma_engine.Env(42)
_ = env.reset()
print("initial hand0 size:", env.hand_size(0))

# Assume for smoke test we discard a tile we know: 
# e.g., Tile ID 0 corresponds to Man1—if it's in your hand, this will work.
obs, r, done = env.step(0)
print("after discarding Man1, hand0 size:", env.hand_size(0))
