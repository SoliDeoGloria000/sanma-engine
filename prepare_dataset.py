# data/prepare_dataset.py
import os
import glob

try:
    import numpy as np
except ImportError:
    print("\n--- Missing Dependency ---")
    print("NumPy is required. Please run 'pip install -r requirements.txt'.")
    raise
import re
import json
import traceback
from collections import deque
import contextlib
import sys
import argparse

try:
    # This assumes your compiled Rust library is named 'sanma_engine'
    # and is in the same directory or your PYTHONPATH.
    from sanma_engine import Env
except ImportError as e:
    print("\n--- FATAL ERROR ---")
    print(f"Could not import 'sanma_engine': {e}")
    print("This script requires the Rust game engine to be compiled and accessible.")
    print(
        "Please run 'maturin develop' or 'maturin develop --release' in your project's root directory first."
    )
    exit()

# --- Configuration ---
RAW_LOGS_DIR = "data/raw_logs"
SAVE_DIR = "data/shards"
SHARD_SIZE = 50000  # Number of (observation, action) pairs per .npz file
DEBUG_MODE = True  # Set to False for faster processing of large log sets
# Set to False to see warnings from the Rust engine when debugging dataset
# extraction issues.
SUPPRESS_ENGINE_WARNINGS = False

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


def is_complex_action_id(action_id: int) -> bool:
    """Determine if the given action ID represents a complex call or win."""
    if ACTION_ID_DISCARD_START <= action_id < ACTION_ID_ANKAN_START:
        return False
    if action_id == ACTION_ID_PASS:
        return False
    return (
        action_id
        in {
            ACTION_ID_KITA,
            ACTION_ID_TSUMO_AGARI,
            ACTION_ID_RON_AGARI,
            ACTION_ID_PON,
            ACTION_ID_DAIMINKAN,
        }
        or action_id >= ACTION_ID_ANKAN_START
    )


@contextlib.contextmanager
def suppress_output():
    if not SUPPRESS_ENGINE_WARNINGS:
        yield
        return
    with open(os.devnull, "w") as devnull:
        with contextlib.redirect_stdout(devnull), contextlib.redirect_stderr(devnull):
            yield


# ==============================================================================
# SECTION 1: DATA CONVERSION HELPERS
# ==============================================================================


def tenhou_tile_to_engine_id(tenhou_tile_id: int) -> int:
    """Converts a Tenhou tile ID (e.g., 11-19 for manzu) to our engine's internal ID (0-33)."""
    if tenhou_tile_id == 51:
        return tenhou_tile_to_engine_id(15)
    if tenhou_tile_id == 52:
        return tenhou_tile_to_engine_id(25)
    if tenhou_tile_id == 53:
        return tenhou_tile_to_engine_id(35)

    suit = tenhou_tile_id // 10
    rank = tenhou_tile_id % 10

    if 1 <= suit <= 3:
        return (suit - 1) * 9 + (rank - 1)
    elif suit == 4:
        wind_map = {1: 27, 2: 28, 3: 29, 4: 33}
        dragon_map = {5: 30, 6: 31, 7: 32}
        if rank in wind_map:
            return wind_map[rank]
        if rank in dragon_map:
            return dragon_map[rank]

    raise ValueError(f"Invalid Tenhou tile ID: {tenhou_tile_id}")


def get_initial_state_from_log(round_data):
    """Parses initial hands, dealer, scores, and draw queues for all players from a log round."""
    try:
        initial_hands = [
            [tenhou_tile_to_engine_id(t) for t in round_data[4 + i * 3]]
            for i in range(3)
        ]
        initial_draws = [
            [
                tenhou_tile_to_engine_id(t)
                for t in round_data[5 + i * 3]
                if isinstance(t, int)
            ]
            for i in range(3)
        ]

        return {
            "initial_dealer_idx": round_data[0][0] % 3,
            "initial_honba_sticks": round_data[0][1],
            "initial_scores": round_data[1][:3],
            "initial_hands": initial_hands,
            "initial_draws": initial_draws,
        }
    except (IndexError, ValueError) as e:
        if DEBUG_MODE:
            print(f"  [Debug] Could not parse initial state. Error: {e}")
        return None


def map_log_action_to_rust_id(log_action, env, legal_actions_mask=None):
    """Maps a Tenhou log action to the corresponding Rust action ID.

    If ``legal_actions_mask`` is provided, riichi actions will gracefully fall
    back to a normal discard when riichi is not legal in the engine state.
    """
    if isinstance(log_action, int):
        # Some logs incorrectly use 0 to denote a tsumo-giri (discarding the drawn tile)
        if log_action == 0:
            log_action = 60
        drawn_tile_id = env.get_last_drawn_tile_for_current_player_val()
        try:
            tile_to_discard = (
                drawn_tile_id if log_action == 60 else tenhou_tile_to_engine_id(log_action)
            )
        except ValueError:
            if DEBUG_MODE:
                print(f"  [Debug] Invalid tile id {log_action}")
            return None
        return ACTION_ID_DISCARD_START + tile_to_discard

    if isinstance(log_action, str):
        if "r" in log_action:
            tile_int = int(re.search(r"\d+", log_action).group())
            # Some logs use 0 instead of 60 for tsumo-giri riichi
            if tile_int == 0:
                tile_int = 60
            drawn_tile_id = env.get_last_drawn_tile_for_current_player_val()
            try:
                tile_to_discard = (
                    drawn_tile_id if tile_int == 60 else tenhou_tile_to_engine_id(tile_int)
                )
            except ValueError:
                if DEBUG_MODE:
                    print(f"  [Debug] Invalid tile id {tile_int} in riichi action")
                return None
            riichi_id = ACTION_ID_RIICHI_DISCARD_START + tile_to_discard
            normal_id = ACTION_ID_DISCARD_START + tile_to_discard
            if legal_actions_mask is not None:
                if legal_actions_mask[riichi_id]:
                    return riichi_id
                if legal_actions_mask[normal_id]:
                    return normal_id
            return riichi_id
        if "f" in log_action:
            return ACTION_ID_KITA
        if "a" in log_action:
            match = re.search(r"(\d{2})", log_action)
            if not match:
                return None
            tile_int = int(match.group(1))
nae2aa-codex/fix-issues-processing-dataset-files
            if tile_int == 0:
                tile_int = 60
=======
main
            try:
                tile_id = tenhou_tile_to_engine_id(tile_int)
            except ValueError:
                if DEBUG_MODE:
                    print(f"  [Debug] Invalid tile id {tile_int} in ankan action")
                return None
            return ACTION_ID_ANKAN_START + tile_id
        if "k" in log_action:
            match = re.search(r"(\d{2})", log_action)
            if not match:
                return None
            tile_int = int(match.group(1))
        nae2aa-codex/fix-issues-processing-dataset-files
            if tile_int == 0:
                tile_int = 60
=======
         main
            try:
                tile_id = tenhou_tile_to_engine_id(tile_int)
            except ValueError:
                if DEBUG_MODE:
                    print(f"  [Debug] Invalid tile id {tile_int} in shouminkan action")
                return None
            return ACTION_ID_SHOUMINKAN_START + tile_id
        if "m" in log_action:
            return ACTION_ID_DAIMINKAN
        if "p" in log_action:
            return ACTION_ID_PON
        if "n" in log_action:
            return ACTION_ID_RON_AGARI

    return None


def is_log_action_complex(log_action):
    """Rudimentary check if an action string from the raw log represents a
    complex call or win."""
    if isinstance(log_action, str):
        if any(c in log_action for c in "rpakmnf"):
            return True
        try:
            int(log_action)
            return False
        except ValueError:
            return True
    return False


def count_actions_in_round_raw(round_data):
    """Count simple and complex actions directly from a raw log round."""
    simple = 0
    complex_c = 0
    for i in range(3):
        discards = round_data[6 + i * 3]
        draws = round_data[5 + i * 3]
        for act in discards:
            if is_log_action_complex(act):
                complex_c += 1
            else:
                simple += 1
        for act in draws:
            if isinstance(act, str):
                if is_log_action_complex(act):
                    complex_c += 1
                else:
                    simple += 1
    return simple, complex_c


def count_actions_in_log_file(log_json):
    """Aggregate action counts across all rounds of a log file."""
    total_simple = 0
    total_complex = 0
    for round_data in log_json.get("log", []):
        if (
            round_data
            and isinstance(round_data[-1], list)
            and round_data[-1][0] == "流局"
        ):
            continue
        s, c = count_actions_in_round_raw(round_data)
        total_simple += s
        total_complex += c
    return total_simple, total_complex


# ==============================================================================
# SECTION 2: ROBUST GAME REPLAY LOGIC
# ==============================================================================


def process_round(round_data, env):
    """
    Processes a single round of a game log, replaying it step-by-step
    in the Rust engine to generate (observation, action) pairs.
    """
    obs_action_pairs = []

    initial_state = get_initial_state_from_log(round_data)
    if initial_state is None:
        return []

    try:
        with suppress_output():
            env.reset(
                seed=0,
                initial_draws=initial_state.pop("initial_draws"),
                **initial_state,
            )
    except Exception as e:
        if DEBUG_MODE:
            print(f"  [Debug] Error resetting env: {e}\n{traceback.format_exc()}")
        return []

    # Tenhou logs are messy. Calls ('p', 'm') are in the draw queue, discards ('r', 'f', <int>) are in the discard queue.
    # We'll refer to them as action queues.
    draw_queues = [deque(round_data[5 + i * 3]) for i in range(3)]
    discard_queues = [deque(round_data[6 + i * 3]) for i in range(3)]
    skip_next_draw = [False, False, False]

    turn_limit = 200  # Safety break
    for _ in range(turn_limit):
        current_phase = env.get_game_phase_pystr()

        if current_phase == "RoundOver":
            break

        current_actor_idx = env.current_player_idx_py()
        obs, legal_actions_mask = env.get_obs_and_legal_actions()

        log_action = None

        # --- State-Driven Logic ---

        if current_phase == "PlayerTurnAction":
            # The engine is waiting for the current player to act after a draw.
            # Their action will be in their DISCARD queue.
            # Note: The actual drawn tile is already handled by the engine's internal state.
            # We just need to consume the corresponding draw from our queue to stay in sync.
            if not skip_next_draw[current_actor_idx]:
                if draw_queues[current_actor_idx] and isinstance(
                    draw_queues[current_actor_idx][0], int
                ):
                    draw_queues[current_actor_idx].popleft()  # Consume the draw event
            else:
                skip_next_draw[current_actor_idx] = False

            if discard_queues[current_actor_idx]:
                log_action = discard_queues[current_actor_idx].popleft()
            else:
                if DEBUG_MODE:
                    print(
                        f"  [Debug] Early exit: no discard action for player {current_actor_idx}"
                    )
                break  # No more actions for this player.

        elif current_phase in ["WaitingForCalls", "ProcessingShouminkanChankan"]:
            # A discard just occurred. The engine is waiting for other players to interrupt.
            # An interrupting call ('p', 'm', 'n') is found in the caller's DRAW queue.
            peek_action = (
                draw_queues[current_actor_idx][0]
                if draw_queues[current_actor_idx]
                else None
            )
            if isinstance(peek_action, str) and any(c in peek_action for c in "mpn"):
                # This player is making a call.
                log_action = draw_queues[
                    current_actor_idx
                ].popleft()  # Pop the call action
            else:
                # This player is not making a call, so they pass.
                log_action = "PASS"

        if log_action is None:
            if DEBUG_MODE:
                print(
                    f"  [Debug] Early exit: no log action available in phase {current_phase}"
                )
            break

        rust_action_id = (
            ACTION_ID_PASS
            if log_action == "PASS"
            else map_log_action_to_rust_id(log_action, env, legal_actions_mask)
        )
        if rust_action_id is None:
            if DEBUG_MODE:
                print(f"  [Debug] Could not map log action: '{log_action}'")
            continue

        if not legal_actions_mask[rust_action_id]:
            # Use PASS to stay in sync when the logged action isn't legal
            if legal_actions_mask[ACTION_ID_PASS]:
                rust_action_id = ACTION_ID_PASS
            else:
                continue
        obs_action_pairs.append({"observation": obs.copy(), "action": rust_action_id})

        try:
            with suppress_output():
                _, _, done, _ = env.step(rust_action_id)
            if (
                log_action != "PASS"
                and isinstance(log_action, str)
                and any(c in log_action for c in "fakmp")
            ):
                skip_next_draw[current_actor_idx] = True
            if done:
                if DEBUG_MODE:
                    print(f"  [Debug] Round finished after action '{log_action}'")
                break
        except BaseException as e:
            if DEBUG_MODE:
                print(
                    f"  [Debug] Error stepping env with action '{log_action}' (ID {rust_action_id}): {e}\n{traceback.format_exc()}"
                )
            continue

    return obs_action_pairs


# ==============================================================================
# SECTION 3: MAIN EXECUTION AND SHARDING
# ==============================================================================


def main(keep_pass: bool = False):
    print("Starting dataset preparation using the Rust game engine...")
    os.makedirs(SAVE_DIR, exist_ok=True)
    all_log_files = glob.glob(os.path.join(RAW_LOGS_DIR, "*.json"))

    if not all_log_files:
        print(
            f"Error: No .json files found in '{RAW_LOGS_DIR}'. Please add Tenhou logs to this directory."
        )
        return

    print(f"Found {len(all_log_files)} log file(s) to process.")
    all_pairs = []
    shard_num = 0
    env = Env()
    total_simple = 0
    total_complex = 0

    total_pass = 0

    for i, log_file_path in enumerate(all_log_files):
        print(
            f"Processing file {i+1}/{len(all_log_files)}: {os.path.basename(log_file_path)}..."
        )
        file_pair_count = 0
        file_simple = 0
        file_complex = 0
        file_pass = 0
        log_simple = 0
        log_complex = 0
        try:
            with open(log_file_path, "r", encoding="utf-8") as f:
                log_file_json = json.load(f)
            log_simple, log_complex = count_actions_in_log_file(log_file_json)
            for round_idx, round_data in enumerate(log_file_json.get("log", [])):
                if round_data[-1][0] == "流局":
                    continue

                expected_simple, expected_complex = count_actions_in_round_raw(round_data)
                expected_total = expected_simple + expected_complex
                round_pairs = process_round(round_data, env)
                if len(round_pairs) != expected_total:
                    diff = expected_total - len(round_pairs)
                    print(
                        f"  [Warning] Round {round_idx}: parsed {len(round_pairs)} of {expected_total} actions (diff {diff})"
                    )
                for p in round_pairs:
                    if p["action"] == ACTION_ID_PASS:
                        file_pass += 1
                        if keep_pass:
                            all_pairs.append(p)
                            file_pair_count += 1
                            total_pass += 1
                        continue
                    all_pairs.append(p)
                    file_pair_count += 1
                    if is_complex_action_id(p["action"]):
                        total_complex += 1
                        file_complex += 1
                    else:
                        total_simple += 1
                        file_simple += 1
        except Exception as e:
            print(
                f"  [Error] Could not process file '{os.path.basename(log_file_path)}'. Error: {e}"
            )
            if DEBUG_MODE:
                traceback.print_exc()
            continue

        logged_total = log_simple + log_complex
        effective_pairs = file_pair_count
        extraction_rate = effective_pairs / logged_total if logged_total else 0.0
        print(
            f"  --> Extracted {file_pair_count} pairs from this file. "
            f"Simple: {file_simple}, Complex: {file_complex}, Passes: {file_pass}"
        )
        print(
            f"      Original log actions - Simple: {log_simple}, Complex: {log_complex}"
        )
        rate_label = "including passes" if keep_pass else "excluding passes"
        print(f"      Extraction rate ({rate_label}): {extraction_rate:.2%}")

        while len(all_pairs) >= SHARD_SIZE:
            pairs_to_save = all_pairs[:SHARD_SIZE]
            all_pairs = all_pairs[SHARD_SIZE:]
            observations = np.array(
                [p["observation"] for p in pairs_to_save], dtype=np.uint8
            )
            actions = np.array([p["action"] for p in pairs_to_save], dtype=np.uint8)
            shard_path = os.path.join(SAVE_DIR, f"shard_{shard_num:04d}.npz")
            np.savez_compressed(shard_path, observations=observations, actions=actions)
            print(
                f"\n----> Saved shard {shard_num:04d} with {len(actions)} pairs to {shard_path}"
            )
            shard_num += 1

    if all_pairs:
        observations = np.array([p["observation"] for p in all_pairs], dtype=np.uint8)
        actions = np.array([p["action"] for p in all_pairs], dtype=np.uint8)
        shard_path = os.path.join(SAVE_DIR, f"shard_{shard_num:04d}.npz")
        np.savez_compressed(shard_path, observations=observations, actions=actions)
        print(
            f"\n----> Saved final shard {shard_num:04d} with {len(actions)} pairs to {shard_path}"
        )

    total_pairs = total_simple + total_complex + (total_pass if keep_pass else 0)
    if total_pairs:
        ratio = total_complex / (total_pairs - (total_pass if keep_pass else 0)) if (total_pairs - (total_pass if keep_pass else 0)) else 0
        print(
            f"\nCollected {total_pairs} pairs (Simple: {total_simple}, Complex: {total_complex}, Passes: {total_pass}, Complex ratio: {ratio:.2%})"
        )

    print("\nDataset preparation finished.")


if __name__ == "__main__":
    if not os.path.exists(RAW_LOGS_DIR):
        os.makedirs(RAW_LOGS_DIR)
        print(
            f"Created directory '{RAW_LOGS_DIR}'. Please place your Tenhou .json log files here."
        )

    parser = argparse.ArgumentParser(
        description="Prepare dataset using the Rust game engine."
    )
    parser.add_argument(
        "--keep-pass",
        action="store_true",
        help="Include PASS actions in the saved dataset",
    )
    args = parser.parse_args()

    main(keep_pass=args.keep_pass)
