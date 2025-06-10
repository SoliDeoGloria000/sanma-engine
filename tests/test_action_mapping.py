import prepare_dataset as pd

def test_regex_mapping():
    assert pd.map_log_action_to_rust_id('p313131', None) == pd.ACTION_ID_PON
    assert pd.map_log_action_to_rust_id('4747p47', None) == pd.ACTION_ID_PON
    assert pd.map_log_action_to_rust_id('f44', None) == pd.ACTION_ID_KITA
