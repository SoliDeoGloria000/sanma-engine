import sanma_engine

env = sanma_engine.Env()
obs = env.reset()
print("obs shape:", obs.shape)

for _ in range(3):
    obs, reward, done = env.step(0)
    print(reward, done)
