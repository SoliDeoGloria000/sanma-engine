import json
from sanma_engine import Env
from prepare_dataset import process_round

def test_process_round_handles_empty_draw_queue():
    with open('data/raw_logs/2024123123gm-00b9-0000-4a3a3639.json') as f:
        log_data = json.load(f)
    round_data = log_data['log'][0]
    round_data[8] = []  # Emulate missing draw events for player 1
    env = Env()
    result = process_round(round_data, env)
    assert isinstance(result, list)
