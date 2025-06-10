import os
import glob
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader, random_split

# --- 1. Model Definition (CNN + MLP Trunk) ---

class SanmaAI(nn.Module):
    def __init__(self, num_actions):
        super(SanmaAI, self).__init__()
        # Input shape: (N, 149, 5, 5)
        self.cnn_backbone = nn.Sequential(
            nn.Conv2d(149, 128, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.Conv2d(128, 64, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.Flatten(),
        )
        
        # Output of the CNN is 64 * 5 * 5 = 1600
        self.policy_head = nn.Sequential(
            nn.Linear(64 * 5 * 5, 512),
            nn.ReLU(),
            nn.Linear(512, num_actions),
        )

    def forward(self, x):
        features = self.cnn_backbone(x)
        policy_logits = self.policy_head(features)
        return policy_logits

# --- 2. Custom Dataset for loading .npz shards ---

class SanmaExpertDataset(Dataset):
    def __init__(self, shard_files):
        self.shard_files = shard_files
        self.data = []
        for f in shard_files:
            try:
                shard = np.load(f)
                # Ensure observations are float32 for the model and actions are long for the loss function
                self.data.append({
                    'observations': torch.from_numpy(shard['observations']).float(),
                    'actions': torch.from_numpy(shard['actions']).long()
                })
            except Exception as e:
                print(f"Could not load or process shard {f}: {e}")
        
        # Concatenate all data into single tensors
        self.all_observations = torch.cat([d['observations'] for d in self.data], dim=0)
        self.all_actions = torch.cat([d['actions'] for d in self.data], dim=0)

    def __len__(self):
        return self.all_observations.shape[0]

    def __getitem__(self, idx):
        return self.all_observations[idx], self.all_actions[idx]

# --- 3. Updated Training Loop with Validation ---

def train(data_dir, model_save_path, num_epochs=50, learning_rate=1e-4, batch_size=64, val_split=0.1):
    """Main training function with a validation loop."""
    
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Using device: {device}")

    # Corrected Action Space Size from lib.rs (0 to 141)
    ACTION_SPACE_SIZE = 142 
    model = SanmaAI(num_actions=ACTION_SPACE_SIZE).to(device)
    optimizer = optim.Adam(model.parameters(), lr=learning_rate)
    criterion = nn.CrossEntropyLoss()

    # Load dataset
    shard_files = glob.glob(os.path.join(data_dir, "*.npz"))
    if not shard_files:
        print(f"Error: No .npz shard files found in '{data_dir}'.")
        print("Please run `data/prepare_dataset.py` or place your data there.")
        return

    dataset = SanmaExpertDataset(shard_files)
    
    # Split dataset into training and validation sets
    val_size = int(len(dataset) * val_split)
    train_size = len(dataset) - val_size
    train_dataset, val_dataset = random_split(dataset, [train_size, val_size])
    
    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False)
    
    print(f"Found {len(dataset)} total expert moves.")
    print(f"Training on {len(train_dataset)} samples, validating on {len(val_dataset)} samples.")
    print(f"Starting supervised pre-training for up to {num_epochs} epochs...")

    best_val_accuracy = 0.0

    for epoch in range(num_epochs):
        # --- Training Phase ---
        model.train()
        total_train_loss = 0
        
        for i, (observations, actions) in enumerate(train_loader):
            observations = observations.to(device)
            actions = actions.to(device)

            optimizer.zero_grad()
            policy_logits = model(observations)
            loss = criterion(policy_logits, actions)
            
            loss.backward()
            optimizer.step()
            
            total_train_loss += loss.item()

        avg_train_loss = total_train_loss / len(train_loader)

        # --- Validation Phase ---
        model.eval()
        total_val_loss = 0
        correct_predictions = 0
        total_samples = 0
        
        with torch.no_grad():
            for observations, actions in val_loader:
                observations = observations.to(device)
                actions = actions.to(device)
                
                policy_logits = model(observations)
                loss = criterion(policy_logits, actions)
                total_val_loss += loss.item()
                
                _, predicted_actions = torch.max(policy_logits, 1)
                correct_predictions += (predicted_actions == actions).sum().item()
                total_samples += actions.size(0)

        avg_val_loss = total_val_loss / len(val_loader)
        val_accuracy = (correct_predictions / total_samples) * 100

        print(f"--- Epoch {epoch+1}/{num_epochs} ---")
        print(f"  Train Loss: {avg_train_loss:.4f} | Val Loss: {avg_val_loss:.4f} | Val Accuracy: {val_accuracy:.2f}%")
        
        # Save the model only if validation accuracy has improved
        if val_accuracy > best_val_accuracy:
            best_val_accuracy = val_accuracy
            # Create directory if it doesn't exist
            os.makedirs(os.path.dirname(model_save_path), exist_ok=True)
            torch.save(model.state_dict(), model_save_path)
            print(f"  New best validation accuracy! Saved model to {model_save_path}")

        # Optional: Stop if target accuracy is reached
        if val_accuracy >= 38.0:
            print(f"\nTarget validation accuracy (38%) reached! Stopping training.")
            break
            
    print("\nSupervised pre-training finished.")
    print(f"Best validation accuracy achieved: {best_val_accuracy:.2f}%")


if __name__ == '__main__':
    # Directory where your .npz files are stored
    DATASET_DIR = "data/shards"
    
    # Path to save the final trained model.
    # The script will create the directory if it doesn't exist.
    MODEL_CHECKPOINT_PATH = "my_models/sanma_v1.pt"
    
    train(DATASET_DIR, MODEL_CHECKPOINT_PATH)