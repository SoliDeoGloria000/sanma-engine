# data/prepare_dataset.py
import os
import glob
import numpy as np
import re
import json
import traceback
from collections import deque

try:
    # This assumes your compiled Rust library is named 'sanma_engine'
    # and is in the same directory or your PYTHONPATH.
    from sanma_engine import Env
except ImportError as e:
    print("\n--- FATAL ERROR ---")
    print(f"Could not import 'sanma_engine': {e}")
    print("This script requires the Rust game engine to be compiled and accessible.")
    print("Please run 'maturin develop' or 'maturin develop --release' in your project's root directory first.")
    exit()

# --- Configuration ---
RAW_LOGS_DIR = "data/raw_logs"
SAVE_DIR = "data/shards"
SHARD_SIZE = 50000  # Number of (observation, action) pairs per .npz file
DEBUG_MODE = True  # Set to False for faster processing of large log sets

# --- Action ID Constants (ensure these match lib.rs) ---
NUM_TILE_TYPES = 34
ACTION_ID_DISCARD_START = 0
ACTION_ID_RIICHI_DISCARD_START = ACTION_ID_DISCARD_START + NUM_TILE_TYPES
ACTION_ID_ANKAN_START = ACTION_ID_RIICHI_DISCARD_START + NUM_TILE_TYPES
ACTION_ID_SHOUMINKAN_START = ACTION_ID_ANKAN_START + NUM_TILE_TYPES
ACTION_ID_KITA = ACTION_ID_SHOUMINKAN_START + NUM_TILE_TYPES
ACTION_ID_TSUMO_AGARI = ACTION_ID_KITA + 1
ACTION_ID_RON_AGARI = ACTION_ID_TSUMO_AGARI + 1
ACTION_ID_PON = ACTION_ID_RON_AGARI + 1
ACTION_ID_DAIMINKAN = ACTION_ID_PON + 1
ACTION_ID_PASS = ACTION_ID_DAIMINKAN + 1

# ==============================================================================
# SECTION 1: DATA CONVERSION HELPERS
# ==============================================================================

def tenhou_tile_to_engine_id(tenhou_tile_id: int) -> int:
    """Converts a Tenhou tile ID (e.g., 11-19 for manzu) to our engine's internal ID (0-33)."""
    if tenhou_tile_id == 51: return tenhou_tile_to_engine_id(15)
    if tenhou_tile_id == 52: return tenhou_tile_to_engine_id(25)
    if tenhou_tile_id == 53: return tenhou_tile_to_engine_id(35)
    
    suit = tenhou_tile_id // 10
    rank = tenhou_tile_id % 10
    
    if 1 <= suit <= 3:
        return (suit - 1) * 9 + (rank - 1)
    elif suit == 4:
        wind_map = {1: 27, 2: 28, 3: 29, 4: 33}
        dragon_map = {5: 30, 6: 31, 7: 32}
        if rank in wind_map: return wind_map[rank]
        if rank in dragon_map: return dragon_map[rank]

    raise ValueError(f"Invalid Tenhou tile ID: {tenhou_tile_id}")

def get_initial_state_from_log(round_data):
    """Parses initial hands, dealer, scores, and draw queues for all players from a log round."""
    try:
        initial_hands = [[tenhou_tile_to_engine_id(t) for t in round_data[4 + i * 3]] for i in range(3)]
        initial_draws = [[tenhou_tile_to_engine_id(t) for t in round_data[5 + i * 3] if isinstance(t, int)] for i in range(3)]
        
        return {
            "initial_dealer_idx": round_data[0][0],
            "initial_honba_sticks": round_data[0][1],
            "initial_scores": round_data[1][:3],
            "initial_hands": initial_hands,
            "initial_draws": initial_draws,
        }
    except (IndexError, ValueError) as e:
        if DEBUG_MODE: print(f"  [Debug] Could not parse initial state. Error: {e}")
        return None

def map_log_action_to_rust_id(log_action, env):
    """Maps a Tenhou log action to the corresponding Rust action ID, using game state context."""
    if isinstance(log_action, int):
        drawn_tile_id = env.get_last_drawn_tile_for_current_player_val()
        tile_to_discard = drawn_tile_id if log_action == 60 else tenhou_tile_to_engine_id(log_action)
        return ACTION_ID_DISCARD_START + tile_to_discard

    if isinstance(log_action, str):
        normalized = re.sub(r'^\d+', '', log_action)
        if re.search(r'^p\d+', normalized):
            return ACTION_ID_PON
        if re.search(r'^f\d+', normalized):
            return ACTION_ID_KITA
        if 'r' in normalized:
            tile_int = int(re.search(r'\d+', normalized).group())
            drawn_tile_id = env.get_last_drawn_tile_for_current_player_val()
            tile_to_discard = drawn_tile_id if tile_int == 60 else tenhou_tile_to_engine_id(tile_int)
            return ACTION_ID_RIICHI_DISCARD_START + tile_to_discard
        if 'a' in normalized:
            tile_int = int(re.search(r'a(\d+)', normalized).group(1))
            tile_id = tenhou_tile_to_engine_id(tile_int // 10 * 10 + tile_int % 10)
            return ACTION_ID_ANKAN_START + tile_id
        if 'k' in normalized:
            tile_int = int(re.search(r'k(\d+)', normalized).group(1))
            tile_id = tenhou_tile_to_engine_id(tile_int)
            return ACTION_ID_SHOUMINKAN_START + tile_id
        if 'm' in normalized:
            return ACTION_ID_DAIMINKAN
        if 'p' in normalized:
            return ACTION_ID_PON
        if 'f' in normalized:
            return ACTION_ID_KITA
        if 'n' in normalized:
            return ACTION_ID_RON_AGARI

    return None
